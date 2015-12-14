#######################
## Setup environment
#######################
# setwd("/Users/afenichel14/Documents/Columbia/Statistical Inference and Modeling")

set.seed(1)

#######################0
## Install required packages
#######################
# install.packages('ggplot2', repos = "http://cran.us.r-project.org")
# install.packages('scales', repos = "http://cran.us.r-project.org")
# install.packages('readr', repos = "http://cran.us.r-project.org")
# install.packages('caret', repos = "http://cran.us.r-project.org")

#######################
## Load packages
#######################
library(ggplot2)
# library(readr)
# library(scales)
# library(reshape2)
# library(plyr)
# library(caret)

#######################
## Load and transform data
#######################
train <- read.csv("./data/train.csv")
test  <- read.csv("./data/test.csv")

apply_transformations <- function (data) {
  data$datetime <- as.POSIXct(strftime(data$datetime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
  # Isolate time
  data$times <- as.POSIXct(strftime(data$datetime, format="%H:%M:%S"), format="%H:%M:%S")
  # Isolate year
  data$year <- strftime(data$datetime, format="%Y")
  # Isolate hour
  data$hour  <- strftime(data$datetime, format="%H")
  # Isolate weekday
  data$day   <-strftime(data$datetime,'%A')
  # Isolate date
  data$date <- as.POSIXct(strftime(data$datetime, format="%Y-%m-%d"), format="%Y-%m-%d")
  
  # Convert numeric factors
  data$season <- as.factor(data$season)
  data$holiday <- as.factor(data$holiday)
  data$workingday <- as.factor(data$workingday)
  data$weather <- as.factor(data$weather)
  data$hour <- as.factor(data$hour)
  data$day <- as.factor(data$day)
  data$days.from.start <- (as.Date(data$datetime) - as.Date("2011-01-01"))
  data$hours.from.start <- difftime(data$datetime, as.Date('2011-01-01'), units="hours")
  return(data)
}

train <- apply_transformations(train)
test <- apply_transformations(test)



#######################
## Exploratory Analysis
#######################
par(mfrow=c(2,2))
plot(count~atemp, data=train)
plot(count~humidity, data=train)
plot(count~weather, data=train)
plot(count~windspeed, data=train)

par(mfrow=c(2,2))
# Average count by date
plot(sort(unique(train$date)), tapply(train$count, train$date, mean))
plot(count~hour, data=train)
plot(count~holiday, data=train)
plot(count~season, data=train)

par(mfrow=c(1,1))
plot(count~day, data=train)



train.count.by.date_hour <- aggregate(datetime ~ date, data=train, FUN=length)
ggplot() +
  geom_point(aes(x=date, y=datetime), data=train.count.by.date_hour)+
  ggtitle('Count of hourly observations vs date in training data')

cols <- c('datetime','date')
full.data <- rbind(train[,cols], test[,cols])
full.count.by.date_hour <- aggregate(datetime ~ date, data=full.data, FUN=length)
test.count.by.date_hour <- aggregate(datetime ~ date, data=test, FUN=length)

ggplot() +
  geom_point(aes(x=date, y=datetime), data=full.count.by.date_hour)+
  geom_point(aes(x=date, y=datetime), data=test.count.by.date_hour, color='red', size = I(3))+
  ggtitle('Count of hourly observations vs date in full data set')

sum.by.date_hour <- aggregate(count ~ date, data=train, FUN=sum)
ggplot() + 
  geom_point(data=sum.by.date_hour, aes(x=date, y=count), color='black') + 
  geom_point(data=sum.by.date_hour[train.count.by.date_hour$datetime < 24,], aes(x=date, y=count), color='red', size = I(3)) +
  ggtitle("Sum of count vs date-hour")




#######################
## Wilcox test (IMS CH10)
#######################
# Wilcox test by weekday
# found no significant difference weekday vs weekend with p-value= 0.9679
wilcox.test(count~workingday, data = train)

# Wilcox test by holiday
# found no significant difference weekday vs weekend with p-value= 0.8646
wilcox.test(count~holiday, data = train)

# Kruskal test by weekday
kruskal.test(count ~ day, data=train)

# Kruskal test by weekday for workingday = 1 subset
kruskal.test(count ~ day, data=train, subset=workingday=="1")

# Kruskal test by weekday for workingday = 0 subset
kruskal.test(count ~ day, data=train, subset=workingday=="0")

# Kruskal test by weekday for holiday = 1 subset
kruskal.test(count ~ day, data=train, subset=holiday=="1")

# Kruskal test by hour
kruskal.test(count ~ hour, data=train)

# Kruskal test by hour for workingday = 0 subset
kruskal.test(count~hour, data=train, subset=workingday=="0")

# Kruskal test by hour for workingday = 1 subset
# there are significant differences by hour whether workingday or not indicating independence with p-value < 2.2e-16
kruskal.test(count~hour, data=train, subset=workingday=="1")

# Kruskal test by hour for peak hours
#interestingly, the wilcox test was able to differentiate by hour even accounting for period when peak and non-peak
kruskal.test(count~hour, data=train, subset=hour%in%c("11","12","13"))

# Kruskal test by weather
# there are significant differences across different types of weather
kruskal.test(count ~ weather, data=train)


# Kruskal test by season
# there are significant differences across different seasons
kruskal.test(count ~ season, data=train)

## NOTE - was getting an error - Switched these from wilcox to kruskal - Chris
# there are significant differences across both temp and adjusted temp
kruskal.test(count ~ atemp, data=train)
kruskal.test(count ~ temp, data=train)

# there are significant differences across humidity
kruskal.test(count ~ humidity, data=train)


kruskal.test(count ~ windspeed, data=train)




#######################
## ANOVA
#######################
summary(aov(count ~ day, data=train))

summary(aov(count ~ hour, data=train))

summary(aov(count ~ season, data=train))

summary(aov(count ~ weather, data=train))

summary(aov(count ~ day+hour, data=train))

summary(aov(count ~ day+hour+season, data=train))

anova.fit <- aov(log(count + 1) ~ day+hour+season+weather+year, data=train)
summary(anova.fit)

print(model.tables(anova.fit,"means"),digits=3)

par(mfrow=c(2,2))
plot(anova.fit)

#pairwise.t.test(train$count, train$day, p.adjust="bonferroni")




lm.fit1=lm(count~poly(hour,1), data=Bike)
lm.fit2=lm(count~poly(hour,2), data=Bike)
lm.fit3=lm(count~poly(hour,3), data=Bike)
lm.fit4=lm(count~poly(hour,4), data=Bike)
lm.fit5=lm(count~poly(hour,5), data=Bike)
lm.fit6=lm(count~poly(hour,6), data=Bike)
lm.fit7=lm(count~poly(hour,7), data=Bike)
lm.fit8=lm(count~poly(hour,8), data=Bike)
lm.fit9=lm(count~poly(hour,9), data=Bike)
lm.fit10=lm(count~poly(hour,10), data=Bike)

anova(lm.fit1, lm.fit2, lm.fit3, lm.fit4, lm.fit5, lm.fit6, lm.fit7, lm.fit8, lm.fit9, lm.fit10)

plt=as.matrix(cbind(rep(1,n),poly(hour,9)))%*%as.matrix(lm.fit9$coef)
par(mfrow=c(1,2))

plot(hour, count)
plot(hour,plt)





#######################
## Linear regression
#######################

# Apply subset
train.data <- subset(train, select=-c(casual, registered, datetime, times, year, date, workingday, hours.from.start, temp))

# Specify functional form
formula <- as.formula(count~.)

# Simple linear regression
train.lm.fit <- lm(formula, data=train.data)
summary(train.lm.fit)
#######################
## Forward selection
#######################
library(leaps)

train.regfit <- regsubsets(formula, data=train.data, nvmax=NULL, nbest=1, method='forward')
plot(train.regfit)

train.model.mat <- model.matrix(formula, data=train.data)

#######################
## K-fold Cross validation
#######################
library(caret)

set.seed(1)
k.cv = 5
p <- dim(train.model.mat)[2] - 1
n <- dim(train.data)[1]
train.val.errors <- matrix(NA, p , k.cv)
folds <- createFolds(c(1:n), k=k.cv, list=TRUE, returnTrain=FALSE)

for(j in 1:k.cv){
  fold_index <- folds[[j]]
  train.regfit <- regsubsets(formula, data=train.data[-fold_index,], nvmax=p, nbest=1, method='forward')
  p1 <- train.regfit$nvmax - 1
  for(i in 1:p1){
    coefi <- coef(train.regfit, id=i)
    train.mat <- train.model.mat[fold_index,names(coefi)]
    pred <- as.matrix(train.mat) %*% as.matrix(coefi)
    train.val.errors[i,j] <- mean((train$count[fold_index]-pred)^2)
  }
}



regfit.mse=apply(train.val.errors,1,mean)
which.min(regfit.mse)
plot(regfit.mse)


# Extract the best model
best.coefi=coef(train.regfit, id=which.min(regfit.mse))
bestfit.mat=train.model.mat[,names(best.coefi)]
lm.bestfit=lm(train$count~.-1,data=data.frame(bestfit.mat))
summary(lm.bestfit)
## R^2 and Adjust R^2 are both 68%, which 





#######################
## Splines
#######################
library(splines)

variable <- 'temp'
# Get range of temperature variable
var.lims <- range(train[,variable])
var.seq <- seq(from=var.lims[1], to=var.lims[2])

var.spline.fit <- smooth.spline(train[,variable], train$count, cv=TRUE)
var.spline.fit$df
# lines(fit.s, lwd=2, col='green')

var.spline.predict <- predict(var.spline.fit, newdata=list(var.seq),se=T)

res<-(var.spline.fit$yin - var.spline.fit$y)/(1-var.spline.fit$lev)
sigma<-sqrt(var(res))
upper<-var.spline.fit$y+2.0*sigma*sqrt(var.spline.fit$lev)
lower<-var.spline.fit$y-2.0*sigma*sqrt(var.spline.fit$lev)


par(mfrow=c(1,1))
plot(train$count~train[,variable])
lines(var.spline.predict$y, lwd=2, col='green')
lines(upper, lwd=2, col='grey', lty=2)
lines(lower, lwd=2, col='grey', lty=2)



#######################
## GAM
#######################
library(gam)
gam.casual=gam(registered~s(temp), data=Bike, se=TRUE)
par(mfrow=c(2,2))
plot(gam.casual)
summary(gam.casual)
names(gam.casual)

gam.reg=gam(registered~s(temp)+workingday+season+s(hour), data=rTrain, se=TRUE)
par(mfrow=c(2,2))
plot(gam.reg)
summary(gam.reg)
names(gam.reg)

