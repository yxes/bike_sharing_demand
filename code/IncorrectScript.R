#### initial setup
rm(list = ls())
getwd()
setwd('/Users/hawooksong/Desktop/bike_sharing_demand')



#### load libraries
library(caTools)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(rpart)
library(rpart.plot)  
library(corrplot)
library(plyr)



#### load data
missTypes <- c(NA, '', ' ')
trainRaw <- read.csv('./data/train.csv', na.strings=missTypes, stringsAsFactors=FALSE)
test <- read.csv('./data/test.csv', na.strings=missTypes, stringsAsFactors=FALSE)



#### data fields
# datetime - hourly date + timestamp  
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 
# 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals



#### quick examination
range(trainRaw$datetime)
table(trainRaw$season)
table(trainRaw$holiday)
table(trainRaw$workingday)
table(trainRaw$weather)
range(trainRaw$temp)
range(trainRaw$atemp)
range(trainRaw$windspeed)



#### preprocessing
## convert weather outlier 
imputeWeatherOutlier <- function(df) {
  df$weather[df$weather==4] <- 3
  return(df)
}
trainRaw <- imputeWeatherOutlier(trainRaw)
test <- imputeWeatherOutlier(test)


## relevel days of the week factor variable
relevelDaysOfWeek <- function(dayVector) {
  days <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  dayVector <- factor(dayVector, levels = days)
  return(dayVector)
}

## function to add day type to df
addDayType <- function(df) {
  colnames <- colnames(df)
  if (!'day' %in% colnames) {
    if ('date' %in% colnames) {
      df$date <- as.Date(df$date)
      df$day <- weekdays(df$date, abbreviate=TRUE)
    } else if ('datetime' %in% colnames) {
      df$datetime <- as.POSIXct(df$datetime)
      df$day <- weekdays(df$datetime, abbreviate=TRUE)
    }
  }
  
  weekends <- c('Sat', 'Sun', 'Saturday', 'Sunday')
  df$dayType <- ifelse(df$day %in% weekends, 'weekend', 'weekday')
  df$dayType <- as.factor(df$dayType)
  
  ## project only the original colnames and dayType column
  df <- df[ , c(colnames, 'dayType')]
  
  ## return
  return(df)
}

## factorize some of the columns
factorize <- function(df) {
  df$season <- factor(df$season) 
  df$holiday <- factor(df$holiday)
  df$workingday <- factor(df$workingday)
  df$weather <- factor(df$weather)
  return(df)  
}
trainRaw <- factorize(trainRaw)
test <- factorize(test)

## function to process and extract more info from datetime
unzipDateTime <- function(df) {

  ## format datetime
  df$datetime <- as.character(df$datetime)  # initialize (in case some other tz info has already been inserted, in which case it will not be over-written)
  df$datetime <- as.POSIXct(df$datetime, tz='America/New_York')

  ## get date
  df$date <- as.Date(df$datetime, tz='America/New_York')
  
  ## get year
  df$year <- as.integer(format(df$datetime, '%Y'))
  #df$year <- as.factor(format(df$datetime, '%Y'))
  
  ## get month
  #df$month <- as.integer(format(df$datetime, '%m'))
  df$month <- as.factor(format(df$datetime, '%m'))
  
  ## get date
  df$dateIndex <- as.integer(format(df$datetime, '%d'))
  
  ## get day of week
  df$day <- weekdays(df$datetime, abbr=TRUE)
  df$day <- relevelDaysOfWeek(df$day)  
  
  ## get time (hour)
  #df$hour <- as.integer(format(df$datetime, '%H'))
  df$hour <- as.factor(format(df$datetime, '%H'))
    
  ## remove datetime column
  #df$datetime <- NULL
  
  ## return
  return(df)
}
trainRaw <- unzipDateTime(trainRaw)
test <- unzipDateTime(test)

## round up windspeed measurements to an integer
length(unique(trainRaw$windspeed))
unique(trainRaw$windspeed)

roundWindspeed <- function(df) {
  df$windspeed <- round(df$windspeed)
  return(df)
}
trainRaw <- roundWindspeed(trainRaw)
test <- roundWindspeed(test)



#### split trainRaw into train and cross-validation datasets
## first split method
# set.seed(123)
# splitCond <- sample.split(trainRaw$count, SplitRatio=c(0.75))
# train <- trainRaw[splitCond==TRUE, ]
# cv <- trainRaw[splitCond==FALSE, ]

## second split method
table(trainRaw$dateIndex)
table(test$dateIndex)

splitCond <- trainRaw$dateIndex <= 14
train <- trainRaw[splitCond==TRUE, ]
cv <- trainRaw[splitCond==FALSE, ]



#### functions to check accuracy
## calculate the root mean squared error (RMSE)
calcRMSE <- function(predictions, observations) {
  RMSE <- sqrt(sum((observations - predictions)^2, na.rm=TRUE) / length(predictions))
  return(RMSE)
}

## calculate the mean absolute error (MAE)
calcMAE <- function(predictions, observations) {
  MAE <- sum(abs(observations - predictions)) / length(predictions)
  return(MAE)
}

## calculate the RMSLE
calcRMSLE <- function(predictions, observations) {
  n <- length(predictions)
  RMSLE <- sqrt(1/n * sum((log(predictions + 1) - log(observations + 1))^2))
  return(RMSLE)
}



######## perform exploratory analysis




######## feature extraction
# daily_agg <- aggregate(cbind(windspeed, temp, humidity) ~ date, data=train, FUN=mean)

## windspeed over 40 mph
train$w40 <- ifelse(train$windspeed >= 40, T, F)
cv$w40 <- ifelse(cv$windspeed >= 40, T, F)

## temperature over 35
train$t35 <- ifelse(train$temp >= 35, T, F)
cv$t35 <- ifelse(cv$temp >= 35, T, F)

## daily average windspeed over 20
## daily average temperature over 30
## daily average humidity over 75
train <- ddply(train, 'date', function(x) {
  x$davg_w20 <- ifelse(mean(x$windspeed) >= 20, T, F)
  x$davg_t30 <- ifelse(mean(x$temp) >= 30, T, F)
  x$davg_h75 <- ifelse(mean(x$humidity) >= 75, T, F)
  x
})

cv <- ddply(cv, 'date', function(x) {
  x$davg_w20 <- ifelse(mean(x$windspeed) >= 20, T, F)
  x$davg_t30 <- ifelse(mean(x$temp) >= 30, T, F)
  x$davg_h75 <- ifelse(mean(x$humidity) >= 75, T, F)
  x  
})



#### running VIF
names(train)
trainNum <- train[ , c('season', 'holiday', 'workingday', 'weather', 'temp', 'atemp', 'humidity', 'windspeed', 
                'year', 'month', 'day', 'hour', 'w40', 't35', 'davg_w20', 'davg_t30', 'davg_h75')]
trainNum <- sapply(trainNum, as.numeric)
vars <- vif_func(trainNum)  # variables that are not heavily correlated







######## regression model
# see regression.R










######## others' implementations
# http://brandonharris.io/kaggle-bike-sharing/
# http://www.evanvanness.com/post/100217670076/neuralnet-r-package-neural-network-to-predict



