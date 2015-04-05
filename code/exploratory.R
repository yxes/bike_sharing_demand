###### correlation amongst variables

## continuous (numeric) variables
numericCols <- c('temp', 'atemp', 'humidity', 'windspeed', 'casual', 'registered', 'count', 'month', 'hour')

## pair-wise plot
pairs(train[numericCols], upper.panel=NULL)
# dev.copy(png, './images/pairwise_plot.png')
# dev.off()

## correlation plot
x <- sapply(train[numericCols], as.numeric)  # temporary copy of train dataset with numeric columns casted as numeric and not as factors
corMatrix <- cor(x)
corrplot(corMatrix, method='ellipse', type='lower')
# dev.copy(png, './images/correlation_plot.png')
# dev.off()

## pair-wise correlations for the temperature, humidity, windspeed
pairs(train[ , c('temp', 'atemp', 'humidity', 'windspeed')])
# dev.copy(png, './images/temp_humidity_windspeed_pairwise.png')
# dev.off()
cor(train[ , c('temp', 'atemp', 'humidity', 'windspeed')])




###### aggregation setup
## create long-format train dataset
train_long <- melt(train, measure.vars=measure.vars)

## add day type (weekend vs. weekday)
train_long <- addDayType(train_long)

## define measure variables
measure.vars <- c('casual', 'registered', 'count')



######
## daily average: windspeed, humidity, temperature
daily_avg <- aggregate(cbind(windspeed, humidity, temp) ~ date, data=train, FUN=mean)

## daily sum of ridership
daily_rides_sum_long <- aggregate(value ~ date + variable, data=train_long, FUN=sum)
daily_rides_sum_long <- addDayType(daily_rides_sum_long)
daily_rides_sum_long$month <- as.integer(format(daily_rides_sum_long$date, '%m'))

## merge above two aggregate datasets
daily_rides_and_avg <- merge(daily_rides_sum_long, daily_avg, by='date')
daily_rides_and_avg <- addDayType(daily_rides_and_avg)
daily_rides_and_avg$month <- as.integer(format(daily_rides_and_avg$date, '%m'))

##
day_hour_agg <- aggregate(cbind(count, casual, registered) ~ day + hour, data=train, FUN=mean)
day_hour_agg <- addDayType(day_hour_agg)
day_hour_agg_long <- melt(day_hour_agg, measure.vars=measure.vars)

##
month_day_hour_agg <- aggregate(cbind(count, casual, registered) ~ month + day + hour, data=train, FUN=mean)
month_day_hour_agg <- addDayType(month_day_hour_agg)



###### aggregation: windspeed, work day, and ridership
## barplot of windspeed by month and year (not very useful)
ggplot(train_long) + 
  geom_boxplot(aes(x=month, y=windspeed, fill=season)) + 
  facet_grid(year ~ .)

## windspeed vs. ridership by year and user type
a <- ggplot(train_long, aes(x=windspeed, y=value, color=variable), alpha=0.2) + 
  geom_point(aes(size=value)) + 
  geom_jitter() + 
  facet_grid(year ~ variable) +
  guides(size=F) + 
  ggtitle('Windspeed and Ridership by User Type and Year')

b <- ggplot(train_long, aes(x=windspeed, y=value, color=variable), alpha=0.2) + 
  geom_point(aes(size=value)) + 
  geom_jitter() + 
  facet_grid(workingday ~ variable) + 
  guides(size=F) + 
  ggtitle('Windspeed and Ridership by User Type and Working Day')

grid.arrange(a, b)
dev.copy(png, './images/windspeed_and_ridership_by_user_type_and_working_day.png')
dev.off()
# the top-right corner spaces are empty for all the plots
# which may indicate that high windspeed results in lower ridership
# for both casual and registered users



## windspeed vs. ridership by year, work day, and user type
c <- ggplot(train, aes(x=windspeed, y=casual)) +
  geom_point(aes(size=casual), color='red') + 
  facet_grid(workingday ~ year) + 
  guides(size=F) + 
  ggtitle('Windspeed and Casual Ridership by Year and Working Day')
d <- ggplot(train, aes(x=windspeed, y=registered)) +
  geom_point(aes(size=registered), color='blue') + 
  facet_grid(workingday ~ year) + 
  guides(size=F) + 
  ggtitle('Windspeed and Registered Ridership by Year and Working Day')

grid.arrange(c, d)
dev.copy(png, './images/windspeed_and_ridership_by_year_and_working_day_2.png')
dev.off()

# on non-working days, ridership drops with high windspeed;
# on working days, ridership does not drop with high windspeed;
# this trend is true for both casual and registered users

train_long$windspeed2 <- cut(train_long$windspeed, 4)

## barplot of ridership by windspeed, work day, and user type (not useful)
ggplot(train_long, aes(x=windspeed2, y=value)) + 
  geom_boxplot(aes(fill=as.factor(windspeed2))) + 
  facet_grid(workingday ~ variable) 

avg_rides_by_workday_windspeed_long <- aggregate(value ~ workingday + windspeed + variable, 
                                        data=train_long, FUN=mean)

## windspeed vs. average ridership by work day and user type (useful)
ggplot(avg_rides_by_workday_windspeed_long, 
       aes(x=windspeed, y=value, color=variable)) + 
  geom_point() + 
  facet_grid(workingday ~ variable) +
  ggtitle('Average Ridership by Windspeed, Working Day, and User Type')
dev.copy(png, 'average_ridership_by_windspeed_work_day_and_user_type.png')
dev.off()
# on non-working days, ridership drops with high windspeed;
# on working days, ridership does not drop with high windspeed
# this trend is true for both casual and registered users

## daily average windspeed vs. ridership
ggplot(daily_rides_and_avg, aes(x=windspeed, y=value)) + 
  geom_point() + 
  stat_smooth(method='loess') +
  facet_grid(dayType ~ variable) + 
  ggtitle('Average Windspeed vs. Ridership by Day Type and User Type')
dev.copy(png, './images/avg_windspeed_vs_ridership_by_day_type_and_user_type.png')
dev.off()



###### aggregation: ridership by date and temperature

## ridership over time
g <- ggplot(daily_rides_and_avg, aes(x=date, y=value)) + 
  geom_point() +
  facet_grid(dayType ~ variable) + 
  ggtitle('Ridership over Time by User Type and Work Day')

## average daily temperature over time
h <- ggplot(daily_avg, aes(x=date, y=temp)) + 
  geom_point() + 
  ggtitle('Temperature over Time')

grid.arrange(g, h)
dev.copy(png, './images/ridership_and_temperature_over_time.png')
dev.off()

# ridership resembles the cyclical pattern of the temperatures!!



###### aggregation: ridership by day of the week and hour of the day
## boxplot of ridership by day of the week (not very useful)
ggplot(train_long, aes(x=day, y=value)) + 
  geom_boxplot(aes(fill=day)) +
  facet_grid(. ~ variable)

## boxplot of ridership by day type (not very useful)
ggplot(train_long, aes(x=dayType, y=value)) + 
  geom_boxplot(aes(fill=dayType)) + 
  facet_grid(. ~ variable)

# it seems the ridership drops on the weekends for the registered users
# while it increases for the casual users

## heat map: total, registered, and casual
ggplot(day_hour_agg_long, aes(x=hour, y=day)) + 
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low="white", high="red") + 
  facet_grid(variable ~ .)

## heat map: total 
h <- ggplot(day_hour_agg, aes(x=hour, y=day)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(low="white", high="red") +
  ggtitle('Registered and Casual Ridership by Hour and Day')

## heat map: registered
i <- ggplot(day_hour_agg, aes(x=hour, y=day)) + 
  geom_tile(aes(fill = registered)) + 
  scale_fill_gradient(low="white", high="red") + 
  ggtitle('Registered Ridership by Hour and Day')

## heat map: casual
j <- ggplot(day_hour_agg, aes(x=hour, y=day)) + 
  geom_tile(aes(fill = casual)) + 
  scale_fill_gradient(low="white", high="red") +
  ggtitle('Casual Ridership by Hour and Day')

## heatmap of ridership in respect to time and days of the week (casual vs. registered)
grid.arrange(h, i, j)
dev.copy(png, './images/heatmap_rides_by_hour_and_day.png')
dev.off()

## average ridership by hour, day type, and user type
ggplot(day_hour_agg_long, aes(x=hour, y=value)) +
  geom_line(aes(group=day, color=day), size=2, alpha=0.5) + 
  facet_grid(dayType ~ variable)
dev.copy(png, './images/rides_by_hour_and_day.png')
dev.off()
# many registered users ride to commute to work as well as for pleasure on non-working days;
# majority of casual users ride during non-working days for pleasure;




###### aggregation: ridership by work day and hour of the day (similar pattern as above)
workday_hour_agg_long <- aggregate(value ~ + workingday + hour + variable, data=train_long, FUN=mean)

## average ridership by hour, work day, and user type
k <- ggplot(workday_hour_agg_long, aes(x=hour, y=value)) + 
  geom_line(aes(group=workingday, color=workingday), size=2, alpha=0.5)  +
  facet_grid(workingday ~ variable) +
  ggtitle('Average Ridership by Hour, Work Day, and User Type')

## barplot of ridership by hour, work day, and user type
l <- ggplot(train_long, aes(x=hour, y=value)) + 
  geom_boxplot(aes(fill=hour)) + 
  guides(fill=F) + 
  facet_grid(workingday ~ variable) +
  ggtitle('Ridership by Hour, Work Day, and User Type')

grid.arrange(k, l)
dev.copy(png, './images/ridership_by_hour_work_day_and_user_type.png')
dev.off()

# many registered users ride to commute to work as well as for pleasure on non-working days;
# majority of casual users ride during non-working days for pleasure;




###### aggregation by month, day, and hour
m <- ggplot(month_day_hour_agg, aes(x=hour, y=casual)) + 
  geom_point() + 
  facet_grid(month ~ day) + 
  ggtitle('Average Casual Ridership by Hour, Day of the Week, and Month')
m
dev.copy(png, './images/avg_casual_ridership_by_hour_day_month.png')
dev.off()
# casual riders like to ride on weekends

n <- ggplot(month_day_hour_agg, aes(x=hour, y=registered)) + 
  geom_point() + 
  facet_grid(month ~ day) + 
  ggtitle('Average Registered Ridership by Hour, Day of the Week, and Month')
n
dev.copy(png, './images/avg_registered_ridership_by_hour_day_month.png')
dev.off()
# registered riders like to ride on weekdays to commute to work
# while also enjoy riding on weekends




###### relationship among work days, holidays, and days of the week
days_agg <- aggregate(cbind(holiday, workingday, day) ~ date, data=train, FUN=function(x) unique(x)-1)
# note: day 0 through 6 indicate Monday through Sunday

table(days_agg$workingday, days_agg$day)
table(days_agg$workingday, days_agg$holiday)
table(days_agg$holiday, days_agg$day)

# 'workingday' variable should be able to represent 
# all the information captured by the 'holiday' and 'day' variables



###### aggregation: hour, season, temperature, and ridership
range(train$atemp)
range(train$temp)

## temperature by hour and season (not very useful)
ggplot(train) + 
  geom_point(aes(x=hour, y=temp)) +
  facet_grid(season ~ .)



## temperature, season, weather, ridership (not useful)
ggplot(train_long, aes(x=temp, y=value)) + 
  geom_point(aes(color=season)) + 
  stat_smooth(method='loess') + 
  facet_grid(workingday ~ variable)

ggplot(train_long, aes(x=temp, y=value)) + 
  geom_point(aes(color=weather)) + 
  stat_smooth(method='loess') + 
  facet_grid(workingday ~ variable)

ggplot(train_long, aes(x=temp, y=value, color=workingday), alpha=0.3) + 
  geom_point() + 
  geom_jitter() +
  stat_smooth(method='loess') + 
  facet_grid(weather ~ variable)


# would like to find out: when the temperature gets too hot, does ridership drop?
# from this initial glance of these plots, it doesn't seem so

## CONDUCT FURTHER ANALYSES!!!!!



###### mean temperature vs. ridership (REDO THIS PART!!!!)

## ridership by temperature, day type, and user type
ggplot(daily_rides_and_avg, aes(x=temp, y=value)) + 
  geom_point() + 
  stat_smooth(method='loess') +
  facet_grid(dayType ~ variable) +
  ggtitle('Ridership vs Daily Average Temperature by Day Type and User Type')
dev.copy(png, './images/ridership_vs_daily_avg_temp_by_day_type_and_user_type.png')
dev.off()
# from this plot, it seems if the temperature is too high
# then the ridership drops (possibly inconsistent with our earlier plot, l)




###### humidity and ridership
ggplot(train_long, aes(x=humidity, y=temp)) + 
  geom_point(aes(size=value, alpha=0.1)) + 
  facet_grid(. ~ variable)
# surprising that high-temperature-high-humidity combos don't occur

ggplot(train, aes(x=humidity, y=temp)) +
  geom_point()
# surprising that high temperature, high humidity doesn't occur

## daily average humidity vs. ridership
ggplot(daily_rides_and_avg, aes(x=humidity, y=value)) + 
  geom_point() + 
  stat_smooth(method='loess') +
  facet_grid(dayType ~ variable) + 
  ggtitle('Ridership vs. Average Daily Humidity by Day Type and User Type')
dev.copy(png, './images/ridership_vs_avg_daily_humidity_by_day_type_and_user_type.png')
dev.off()



###### weather and ridership
q <- ggplot(train_long, aes(x=hour, y=value)) + 
  geom_boxplot(aes(fill=hour)) + 
  guides(fill=F) + 
  facet_grid(weather ~ variable)
q
# it seems that 'weather' can be an integer variable instead of a factor variable




###### year and ridership
r <- ggplot(train_long, aes(x=year, y=value)) + 
  geom_boxplot(aes(fill=as.factor(year))) + 
  facet_grid(. ~ variable)
r
dev.copy(png, './images/ridership_by_year_and_user_type.png')
dev.off()
# more ridership in 2012 than in 2011

s <- ggplot(train_long, aes(x=hour, y=value)) + 
  geom_boxplot(aes(fill=hour)) + 
  facet_grid(year ~ variable)
# more ridership in 2012 than in 2011

t <- ggplot(train_long, aes(x=month, y=value)) + 
  geom_boxplot(aes(fill=month)) + 
  facet_grid(year ~ variable)
# more ridership in 2012 than in 2011




###### season and ridership
## box plot of ridership by hour, season, and user type (not very useful)
ggplot(train_long, aes(x=hour, y=value)) + 
  geom_boxplot(aes(fill=season)) + 
  facet_grid(season ~ variable)

## scatter plot of ridership by season, temperature, and user type (not very useful)
ggplot(train_long, aes(x=temp, y=value)) + 
  geom_point(aes(color=season)) + 
  facet_grid(variable ~ .)








###### take-aways
# 1. use either temp or atempt (don't use both)
# 2. use either working or dayType (don't use both)
# 3. create different models, one to predict casual and another for registered
# 4. high windspeed results in lower ridership for both casual and registered riders on working days.
# 5. temperature and ridership are positively correlated.
# 6. ridership drops on weekends for registered users while it increases for casual riders.
# 7. higher ridership in 2012 than in 2011.
