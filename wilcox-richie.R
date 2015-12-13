library(ggplot2)
library(lubridate)
library(readr)
library(scales)
library(reshape2)
library(plyr)
library(caret)
library(gam)

# I created my own read table
# train <- read.csv("./train.csv")

apply_transformations <- function (data) {
  data$est <- as.POSIXct(strftime(ymd_hms(data$datetime, tz='EST'), format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
  # Isolate time
  data$times <- as.POSIXct(strftime(ymd_hms(data$datetime, tz='EST'), format="%H:%M:%S"), format="%H:%M:%S")
  # Isolate hour
  data$hour  <- hour(ymd_hms(data$times))
  # Isolate weekday
  data$day   <- wday(ymd_hms(data$est), label=TRUE)
  # Isolate date
  data$date <- as.POSIXct(strftime(ymd_hms(data$datetime, tz='EST'), format="%Y-%m-%d"), format="%Y-%m-%d")
  # Convert temp from C to F
  data$temp_f <- data$temp*9/5+32
  data$season <- as.factor(data$season)
  data$holiday <- as.factor(data$holiday)
  data$workingday <- as.factor(data$workingday)
  data$weather <- as.factor(data$weather)
  data$hour <- as.factor(data$hour)
  data$day <- as.factor(data$day)
  data$days.from.start <- (as.Date(data$datetime) - as.Date("2011-01-01"))
  return(data)
}

train <- apply_transformations(train)

wilcox.workday <- wilcox.test(count~workingday, data = train)
# found no significant difference weekday vs weekend with p-value= 0.9679
wilcox.holiday <- wilcox.test(count~holiday, data = train)
# found no significant difference weekday vs weekend with p-value= 0.8646

kruskal.day <- kruskal.test(count, day)
kruskal.weekdaybyday <- kruskal.test(count, day, subset=workingday=="1")
kruskal.nonweekdaybyday <- kruskal.test(count, day, subset=workingday=="0")
kruskal.holidaybyday <- kruskal.test(count, day, subset=holiday=="1")

#found a significant difference with p-value = 0.0113

kruskal.hour <- kruskal.test(count,hour)
kruskal.hour.nonworkday <- kruskal.test(count~hour,subset=workingday=="0")
kruskal.hour.workday <- kruskal.test(count~hour,subset=workingday=="1")

# there are significant differences by hour whether workingday or not indicating independence with p-value < 2.2e-16

kruskal.peakweekendhour<-kruskal.test(count~hour,subset=hour%in%c("11","12","13"))

#interestingly, the wilcox test was able to differentiate by hour even accounting for period when peak and non-peak

kruskal.weather <- kruskal.test(count,weather)

# there are significant differences across different types of weather

kruskal.season <- kruskal.test(count, season)

# there are significant differences across different seasons

wilcox.atemp <-wilcox.test(count,atemp)
wilcox.temp <- wilcox.test(count,temp)

# there are significant differences across both temp and adjusted temp
wilcox.humidity <- wilcox.test(count,humidity)

# there are significant differences across humidity

wilcox.windspeed <- wilcox.test(count,windspeed)


