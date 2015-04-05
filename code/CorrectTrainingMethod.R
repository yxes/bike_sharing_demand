#### setup working environment
rm(list = ls())
getwd()
setwd('/Users/hawooksong/Desktop/bike_sharing_demand/')
dir()



#### import library paths
library(lubridate)
library(plyr)
library(caret)
library(doParallel)



#### import data
trainRaw <- read.csv("./data/train.csv")
testRaw <- read.csv("./data/test.csv")




#### define preprcess functions
## relevel days of the week factor variable
relevelDaysOfWeek <- function(dayVector) {
  days <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  dayVector <- factor(dayVector, levels = days)
  return(dayVector)
}

## function to process and extract more info from datetime
unzipDateTime <- function(df) {
  
  ## format datetime
  df$datetime <- as.character(df$datetime)  # initialize (in case some other tz info has already been inserted, in which case it will not be over-written)
  df$datetime <- as.POSIXct(df$datetime, tz='America/New_York')
  
  ## get date
  df$date <- as.Date(df$datetime, tz='America/New_York')
  
  ## get year
  df$year <- as.integer(format(df$datetime, '%Y'))
  
  ## get month
  df$month <- as.integer(format(df$datetime, '%m'))
  
  ## get date
  df$dateIndex <- as.integer(format(df$datetime, '%d'))
  
  ## get day of week
  df$day <- weekdays(df$datetime, abbr=TRUE)
  
  ## get time (hour)
  df$hour <- as.integer(format(df$datetime, '%H'))
  
  ## return
  return(df)
}

## factorize some of the columns
factorize <- function(df) {
  df$season <- factor(df$season) 
  #df$weather <- factor(df$weather)
  
  df$month <- as.factor(df$month)
  df$hour <- as.factor(df$hour)
  df$day <- relevelDaysOfWeek(df$day)  
  
  # df$holiday <- factor(df$holiday)
  # df$workingday <- factor(df$workingday)
  # df$year <- as.factor(format(df$datetime, '%Y'))
  return(df)  
}






#### preprocess train dataset
train <- trainRaw
train$datetime <- as.POSIXct(train$datetime, tz='America/New_York')
train$YearMonth <- format(train$datetime,"%Y-%m",tz="America/New_York") 
train <- unzipDateTime(train)
train <- factorize(train)
train$w40 <- ifelse(train$windspeed >= 40, T, F)
train$t35 <- ifelse(train$temp >= 35, T, F)
train <- ddply(train, 'date', function(x) {
  x$davg_w20 <- ifelse(mean(x$windspeed) >= 20, T, F)
  x$davg_t30 <- ifelse(mean(x$temp) >= 30, T, F)
  x$davg_h75 <- ifelse(mean(x$humidity) >= 75, T, F)
  x
})



#### create cross-validation datasets
table(train$dateIndex)
splitCond <- train$dateIndex <= 14
train_train <- train[splitCond==TRUE, ]  # dates 1 through 14
cv <- train[splitCond==FALSE, ]  # dates 15 through 19




#### preprocess test dataset
test <- testRaw
test$datetime <- as.POSIXct(test$datetime, tz='America/New_York')
test$YearMonth <- format(test$datetime,"%Y-%m",tz="America/New_York") 
test <- unzipDateTime(test)
test <- factorize(test)
test$w40 <- ifelse(test$windspeed >= 40, T, F)
test$t35 <- ifelse(test$temp >= 35, T, F)
test <- ddply(test, 'date', function(x) {
  x$davg_w20 <- ifelse(mean(x$windspeed) >= 20, T, F)
  x$davg_t30 <- ifelse(mean(x$temp) >= 30, T, F)
  x$davg_h75 <- ifelse(mean(x$humidity) >= 75, T, F)
  x  
})



#### setup parallel processing
## number of cores
nCores <- detectCores()

## define the cluster size
cl <- makeCluster(nCores, outfile="cluster-output.log")

## start the cluster
registerDoParallel(cl)



#### how many models are we going to build, and what's the n for each?
ym_train <- count(train, "YearMonth")
ym_train$cumsum_count <- cumsum(ym_train$freq)
ym_train




#### define formulas
formula_sh <- ' ~ workingday + holiday + day +
  weather + season +
  temp + humidity + windspeed + 
  year + month + hour +                       
  w40 + t35 + davg_w20 + davg_t30 + davg_h75 + 
  workingday * hour + 
  workingday * w40 + 
  workingday * t35 + 
  workingday * davg_w20 + 
  workingday * davg_t30 + 
  workingday * davg_h75'

## formula for registered ridership
reg_formula <- as.formula(paste0('registered', formula_sh))
reg_formula

## formula for casual ridership
cas_formula <- as.formula(paste0('casual', formula_sh))
cas_formula



#### build models
## function to turn factor variables to numeric if the number of levels is less than 2
unfactor_single_level_variables <- function(df) {
  factorCols <- colnames(df)[sapply(df, is.factor)]
  for (col in factorCols) {
    if (length(unique(df[ , col]))==1) {
      df[ , col] <- as.integer(df[ , col])
    }
  }
  return(df)
}

## function to create models 
## using the list of all the possible train groups, 
## train models only using current+prior months

create_models <- function(formula, trainDF, utilPriorMonths=F, enableStep=F) {
  models <- lapply(seq_along(ym_train$YearMonth), function(x){
    if (utilPriorMonths) {
      utilYearMonths <- ym_train$YearMonth[1:x]  # utilized year-months include both current and prior year-months
    } else {
      utilYearMonths <- ym_train$YearMonth[x]  # utilized year-month includes only the current year-month
    }
    
    utilYearMonthsStr <- paste0(utilYearMonths, collapse=', ')
    print(paste0('Utilizing the following year-months to build a model: ', utilYearMonthsStr))    
    
    subTrainDF <<- trainDF[trainDF$YearMonth %in% utilYearMonths,]    
    subTrainDF <<- unfactor_single_level_variables(subTrainDF)

    fit <- glm(formula, data=subTrainDF, family='poisson')
    if (enableStep) {fit <- step(fit, direction='both')}
    fit
  })
  return(models)
}


## initial models for casual ridership to test on cross-validation dataset
cas_models <- create_models(cas_formula, train_train, utilPriorMonths=F, enableStep=T)

## initial models for registered ridership to test on cross-validation dataset
reg_models <- create_models(reg_formula, train_train, utilPriorMonths=F, enableStep=T)



######################################################################
# Now create predictions using the models we built and the test data
######################################################################

#### year-month cumulative sum count
ym_cv <- count(cv, 'YearMonth')
ym_cv$cumsum_count <- cumsum(ym_cv$freq)
ym_cv

ym_test <- count(test, "YearMonth")
ym_test$cumsum_count <- cumsum(ym_test$freq)
ym_test



#### function to make predictions from built models
# loop over all the months calling predict for that month's test data
# using the model built for "current month + all prior months"
make_predictions <- function(casModelsList, regModelsList, testDF) {
  outputDF <- do.call(rbind, lapply(seq_along(ym_test$YearMonth), function(x){
    currentYearMonth <- ym_test$YearMonth[x]
    
    subTestDF <- testDF[testDF$YearMonth %in% currentYearMonth, ]
    cas_preds <- round(predict(casModelsList[[x]], subTestDF, type='response'))
    reg_preds <- round(predict(regModelsList[[x]], subTestDF, type='response'))
    tot_preds <- cas_preds + reg_preds
    data.frame(casual=cas_preds, registered=reg_preds, count=tot_preds)
  }))
  return(outputDF)  
}



#### test RMSLE on cross-validation dataset
cvPredDF <- make_predictions(cas_models, reg_models, cv)
dim(cvPredDF)
dim(cv)

calcRMSLE(cvPredDF$casual, cv$casual)
calcRMSLE(cvPredDF$registered, cv$registered)
calcRMSLE(cvPredDF$count, cv$count)



#### build final models and make predictions
## models for casual ridership (production models)
cas_models_final <- create_models(cas_formula, train, utilPriorMonths=F, enableStep=T)

## models for registered ridership (production models)
reg_models_final <- create_models(reg_formula, train, utilPriorMonths=F, enableStep=T)

## prediction output df
predDF <- make_predictions(cas_models_final, reg_models_final, test)
dim(predDF)
dim(test)



#### export predictions
predOutputDF <- predDF
predOutputDF$datetime <- test$datetime
predOutputDF <- predOutputDF[ , c('datetime', 'count')]
head(predOutputDF, 30)
write.csv(predOutputDF, "./data/predictions.csv", row.names=F, quote=F)


