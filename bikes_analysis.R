#######################
## Setup environment
#######################
# setwd("/Users/afenichel14/Documents/Columbia/Statistical Inference and Modeling")

set.seed(1)

#######################
## Install required packages
#######################
# install.packages('ggplot2', repos = "http://cran.us.r-project.org")
# install.packages('scales', repos = "http://cran.us.r-project.org")
# install.packages('readr', repos = "http://cran.us.r-project.org")
# install.packages('leaps', repos = "http://cran.us.r-project.org")
# install.packages('caret', repos = "http://cran.us.r-project.org")
# install.packages('ellipse', repos = "http://cran.us.r-project.org")
# install.packages('gridExtra', repos = "http://cran.us.r-project.org")
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
  data$weather <- factor(data$weather, labels=c('Clear', 'Mist', 'Light Rain/Snow', 'Heavy Rain/Snow'))
  data$hour <- as.factor(data$hour)
  data$day <- factor(data$day, levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'), ordered=TRUE)
  data$days.from.start <- as.integer((as.Date(data$datetime) - as.Date("2011-01-01")))
  data$hours.from.start <- as.integer(difftime(data$datetime, as.Date('2011-01-01'), units="hours"))
  
  
  return(data)
}

train <- apply_transformations(train)
test <- apply_transformations(test)



#######################
## Exploratory Analysis
#######################

# Correlation plot
library(ellipse)
ctab <- cor(train[,c('temp','atemp', 'humidity','windspeed','count','days.from.start')])
round(ctab, 2)
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(ctab, col=rgb(colorfun((ctab+1)/2), maxColorValue=255),
         mar = c(0.1, 0.1, 0.1, 0.1))
n <- nrow(ctab)
for (i in 1:n)
{
  for (j in 1:n)
  {
    text(j,i,round(ctab[n-i+1,j],2),col="black",cex=0.9)     
  }
}


# Scatter plots

library(gridExtra)
p1 <- ggplot(train, aes(atemp, count)) + geom_point(alpha=.15) + ggtitle("Count increases with temperature") + xlab('"Feels like" temperature in C') + ylab('Rental Count')
p2 <- ggplot(train, aes(humidity, count)) + geom_point(alpha=.15) + ggtitle("Count decreases with humidity")  + xlab('Humidity') + ylab('Rental Count')
p3 <- ggplot(train, aes(weather, count)) + geom_boxplot() + ggtitle("Count decreases with bad weather")  + xlab('Weather') + ylab('Rental Count')
p4 <- ggplot(train, aes(windspeed, count)) + geom_point(alpha=.15) + ggtitle("Count decreases with windspeed")  + xlab('Windspeed') + ylab('Rental Count')
p5 <- ggplot(train, aes(hour, count)) + geom_boxplot() + ggtitle("Count spikes during commute times") + xlab('Hour') + ylab('Rental Count') 
p6 <- ggplot(train, aes(holiday, count)) + geom_boxplot() + ggtitle("Count drops on holidays") + xlab('Holiday') + ylab('Rental Count')
p7 <- ggplot(train, aes(season, count)) + geom_boxplot() + ggtitle("Count is largest in the summer") + xlab('Season') + ylab('Rental Count')
p8 <- ggplot(train, aes(day, count)) + geom_boxplot() + ggtitle("Count decreases on weekends") + xlab('Weekday') + ylab('Rental Count')
grid.arrange(p1, p2, ncol=2)
grid.arrange(p3, p4, ncol=2)
grid.arrange(p5, p6, ncol=2)
grid.arrange(p7, p8, ncol=2)

# Average count by date
ggplot() + geom_point(aes(x=sort(unique(train$date)), y=tapply(train$count, train$date, mean)))  + ggtitle("Average count by date increases over time") + xlab('Date') + ylab('Rental Count')

# Average count by weekday and hour
ggplot(aggregate(count~hour+day, data=train, FUN=mean), aes(x=hour, y=count)) +  
  geom_line(aes(color=day, group=day)) +
  ggtitle('Average count by weekday and hour') +
  xlab('Hour') +
  ylab('Average count')


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




windspeed.0=windspeed[windspeed>0]
lm.fit1=lm(count~poly(windspeed,1), data=train)
lm.fit2=lm(count~poly(windspeed,2), data=train)
lm.fit3=lm(count~poly(windspeed,3), data=train)
lm.fit4=lm(count~poly(windspeed,4), data=train)
lm.fit5=lm(count~poly(windspeed,5), data=train)
lm.fit6=lm(count~poly(windspeed,6), data=train)
lm.fit7=lm(count~poly(windspeed,7), data=train)
lm.fit8=lm(count~poly(windspeed,8), data=train)
lm.fit9=lm(count~poly(windspeed,9), data=train)

anova(lm.fit1, lm.fit2, lm.fit3, lm.fit4, lm.fit5, lm.fit6, lm.fit7, lm.fit8, lm.fit9)

n.wind=length(windspeed.0)
plt=as.matrix(cbind(rep(1,n.wind),poly(windspeed.0,6)))%*%as.matrix(lm.fit6$coef)
par(mfrow=c(1,2))

plot(windspeed.0, count[windspeed>0])
plot(windspeed.0,plt)






#######################
## Linear regression
#######################
  
# Remove outlier with 1 observation
train <- train[train$weather != 'Heavy Rain/Snow',]
# Apply subset
train.data <- subset(train, select=-c(casual, registered, datetime, times, year, date, workingday, hours.from.start, temp))
  
# Specify functional form
formula <- as.formula(count~.)
  
# Simple linear regression
train.lm.fit <- lm(formula, data=train.data)
summary(train.lm.fit)
#######################
## Forward selection with K-fold Cross validation
#######################
library(leaps)
library(caret)
  
train.model.mat <- model.matrix(formula, data=train.data)
  
set.seed(1)
k.cv = 10
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
  
  

which.min(regfit.mse)
regfit.mse[which.min(regfit.mse)]
par(mfrow=c(1,1))
plot(regfit.mse)


# Extract the best model
best.coefi=coef(train.regfit, id=which.min(regfit.mse))
bestfit.mat=train.model.mat[,names(best.coefi)]
lm.bestfit=lm(train$count~.-1,data=data.frame(bestfit.mat))
summary(lm.bestfit)

par(mfrow=c(2,2))
plot(lm.bestfit)


#### Forward selection with log transformed response variable ####

# Specify functional form
formulalog <- as.formula(log(count)~.)

# Simple linear regression
train.lm.fit <- lm(formulalog, data=train.data)
summary(train.lm.fit)
#######################
## Forward selection with K-fold Cross validation
#######################
library(leaps)
library(caret)

train.model.mat <- model.matrix(formulalog, data=train.data)

set.seed(1)
k.cv = 10
p <- dim(train.model.mat)[2] - 1
n <- dim(train.data)[1]
train.val.errors <- matrix(NA, p , k.cv)
folds <- createFolds(c(1:n), k=k.cv, list=TRUE, returnTrain=FALSE)

for(j in 1:k.cv){
  fold_index <- folds[[j]]
  train.regfit <- regsubsets(formulalog, data=train.data[-fold_index,], nvmax=p, nbest=1, method='forward')
  p1 <- train.regfit$nvmax - 1
  for(i in 1:p1){
    coefi <- coef(train.regfit, id=i)
    train.mat <- train.model.mat[fold_index,names(coefi)]
    pred <- as.matrix(train.mat) %*% as.matrix(coefi)
    train.val.errors[i,j] <- mean((train$count[fold_index]-exp(pred))^2)
  }
}


which.min(regfit.mse)
regfit.mse[which.min(regfit.mse)]
par(mfrow=c(1,1))
plot(regfit.mse)


# Extract the best model
best.coefi=coef(train.regfit, id=which.min(regfit.mse))
bestfit.mat=train.model.mat[,names(best.coefi)]
lm.bestfit=lm(log(train$count)~.-1,data=data.frame(bestfit.mat))
summary(lm.bestfit)

par(mfrow=c(2,2))
plot(lm.bestfit)



# Models with transformations
library(boot)
# 3726 MSE
# 0.89 adj r^2
f <- as.formula(count~(atemp+humidity+windspeed+days.from.start+holiday+day)*hour)
rf <- glm(f, data=train)
cv.glm(train, rf, K = 5)$delta[1]
summary(lm(f, data=train))

f <- as.formula(log(count)~(atemp+humidity+windspeed+days.from.start+holiday+day)*hour)
rf <- glm(f, data=train)
cv.glm(train, rf, K = 10)$delta[1]
summary(lm(f, data=train))




f <- as.formula(count~season+atemp+humidity+windspeed+hour+day+days.from.start+weather)
#rf <- glm(f, data=train)
#cv.glm(train, rf, K = 7)$delta[1]
summary(lm(f, data=train))

f <- as.formula(log(count)~(atemp+humidity+windspeed+days.from.start+holiday+day*hour))
rf <- glm(f, data=train)
cv.glm(train, rf, K = 5)$delta[1]




#######################
## Splines
#######################
library(splines)

variable <- 'atemp'
# Get range of temperature variable
var.lims <- range(train[,variable])
#var.seq <- seq(from=var.lims[1], to=var.lims[2], by='h')
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
#lines(var.spline.fit, lwd=2, col='green')
lines(var.spline.predict$y, lwd=2, col='green')
lines(upper, lwd=2, col='grey', lty=2)
lines(lower, lwd=2, col='grey', lty=2)



#######################
## GAM
#######################
library(mgcv)
library(gamclass)
form <- as.formula(log(count)~s(as.integer(hour))+s(humidity)+s(temp)+s(windspeed)+s(windspeed)+s(as.integer(days.from.start))
gam.fit <- gam(form, data=train)
par(mfrow=c(2,2))
plot(gam.fit)
summary(gam.fit)

CVgam(form, data=train, nfold=10, seed=1)





### Kaggle submission
pred <- predict(rf, test)
write.table(cbind(test$datetime, data.frame(exp(pred))), quote=FALSE, file='testing.csv', sep=',', row.names=FALSE, col.names=c('datetime','count'))
  
