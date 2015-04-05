library(gbm)
library(randomForest)
library(lubridate)

# Load Training data
train = read.csv("train.csv")
test = read.csv("test.csv")

###################################################
## Step 1: Data Cleaning and Tiding
###################################################

# set categorical variables ####
# season, holiday, workingday, weather
train$season <- factor(train$season, c(1,2,3,4), ordered=FALSE)
train$holiday <- factor(train$holiday, c(0,1), ordered=FALSE)
train$workingday <- factor(train$workingday, c(0,1), ordered=FALSE)
train$weather <- factor(train$weather, c(4,3,2,1), ordered=TRUE)
# set datetime ####
train$datetime <- as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")
str(train)

test$season <- factor(test$season, c(1,2,3,4), ordered=FALSE)
test$holiday <- factor(test$holiday, c(0,1), ordered=FALSE)
test$workingday <- factor(test$workingday, c(0,1), ordered=FALSE)
test$weather <- factor(test$weather, c(4,3,2,1), ordered=TRUE)
# set datetime ####
test$datetime <- as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M:%S")
#Compute hour of the day
train$hour <-hour(train$datetime)
test$hour <- hour(test$datetime)
#Compute day of the week
train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)
test$count<-0

###################################################
## Step 1: Training Data with GBM
###################################################

#Tune the model finding the best mtry value in this case 13 with 
#the lower OOB error
rf.tune <- tuneRF(train[,-12],train[,12],stepFactor=1.5)

Sys.time() #Start Time

fit <- randomForest(count ~ season + holiday + weather + dow+ hour + temp + atemp+humidity+windspeed , data=train, ntree = 2500,importance=TRUE, mtry = 13)

#save(fit, file="randomforestmodel.rda") #if you want to save the model

Sys.time() #Finish Time


#Predict values and save output
Prediction = predict(fit, newdata = test)
Prediction <- abs(Prediction)
table(is.na(Prediction))
Prediction[is.na(Prediction)] <- mean(Prediction, na.rm=TRUE)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "random-forest1500try13.csv", row.names = FALSE)

#Results:
# ntree = 2500 ; mtry = 13 -> better result 0,482
# ntree = 3500 ; mtry = 13 -> worst result - 45 minutes of execution
# ntree = 1500 ; mtry = 13 -> worst result - 16 minutes of execution
