setwd("/Users/afenichel14/Documents/Columbia/Statistical Inference and Modeling")
f="train.csv"
Bike=read.csv(f)
Bike$season=as.factor(Bike$season)
Bike$holiday=as.factor(Bike$holiday)
Bike$workingday=as.factor(Bike$workingday)
Bike$weather=as.factor(Bike$weather)

## Initialize data
datetime=as.POSIXct(strptime(Bike$datetime, '%Y-%m-%d %H:%M:%S'))
hour=as.integer(format(datetime, format='%H'))
year=as.integer(format(datetime, format='%Y'))
Bike=data.frame(Bike, hour, year)
attach(Bike)
names(Bike)

n=dim(Bike)[1]
p=dim(Bike)[2]

set.seed(1)
##BikeSample=Bike[sample(1:n,5000),]

### REGISTERED USERS ###
## Find Best Subset:
lm.fit=lm(registered~., data=subset(Bike, select=-c(casual, count, datetime)))
summary(lm.fit)
library(leaps)
regfit=regsubsets(registered~., data=subset(Bike, select=-c(casual, count, datetime)), nvmax=p)
summary(regfit)

model.mat=model.matrix(registered~., data=subset(Bike, select=-c(casual, count, datetime)))


## find best subset data matrix
library(caret)
set.seed(1)
k.cv = 5


val.errors=matrix(NA,p,k.cv)
folds <- createFolds(c(1:n), k=k.cv, list=TRUE, returnTrain=FALSE)
for(i in 1:p){
  coefi=coef(regfit, id=i)
  train.mat=model.mat[,names(coefi)]
  for(j in 1:k.cv){
    test=folds[[j]]
    lm.fit=lm(registered[-test]~.-1, data=data.frame(train.mat[-test,]))
    pred=as.matrix(train.mat[test,])%*%matrix(lm.fit$coef)
    val.errors[i,j]=mean((as.matrix(registered[test])-pred)^2)
  }
}
regfit.mse=apply(val.errors,1,mean)
which.min(regfit.mse)
## The best model is the one with 10 coefficients
plot(regfit.mse)
coefi=coef(regfit, id=10)
bestfit.mat=model.mat[,names(coefi)]
lm.bestfit=lm(registered~.-1,data=data.frame(bestfit.mat))
summary(lm.bestfit)
## R^2 and Adjust R^2 are both 68%, which 



## ANOVA
lm.fit1=lm(registered~poly(hour,1), data=Bike)
lm.fit2=lm(registered~poly(hour,2), data=Bike)
lm.fit3=lm(registered~poly(hour,3), data=Bike)
lm.fit4=lm(registered~poly(hour,4), data=Bike)
lm.fit5=lm(registered~poly(hour,5), data=Bike)
lm.fit6=lm(registered~poly(hour,6), data=Bike)
lm.fit7=lm(registered~poly(hour,7), data=Bike)
lm.fit8=lm(registered~poly(hour,8), data=Bike)

anova(lm.fit1, lm.fit2, lm.fit3, lm.fit4, lm.fit5, lm.fit6, lm.fit7, lm.fit8)

plt=as.matrix(cbind(rep(1,n),poly(hour,6)))%*%as.matrix(lm.fit6$coef)
par(mfrow=c(2,1))
plot(hour,plt)
plot(hour, registered)


## USING SPLINES
library(splines)
par(mfrow=c(1,1))
plot(temp,registered)
templims=range(temp)
temp.grid=seq(from=templims[1],to=templims[2])
fit.s=smooth.spline(temp, registered, cv=TRUE)
pred=predict(fit.s, newdata=list(temp=temp.grid),se=T)
fit.s$df
res=(fit.s$yin-fit.s$y)/(1-fit.s$lev)
sigma=sqrt(var(res))
upper=fit.s$y+2.0*sigma*sqrt(fit.s$lev)
lower=fit.s$y-2.0*sigma*sqrt(fit.s$lev)

plot(temp,registered)
lines(pred$y, lwd=2, col='green')
lines(upper, lwd=2, col='grey', lty=2)
lines(lower, lwd=2, col='grey', lty=2)



####################

### CASUAL USERS ###
## Find Best Subset:
lm.fit=lm(casual~., data=subset(Bike, select=-c(registered, count, datetime)))
summary(lm.fit)
library(leaps)
regfit=regsubsets(casual~., data=subset(Bike, select=-c(registered, count, datetime)), nvmax=p)
summary(regfit)

model.mat=model.matrix(casual~., data=subset(Bike, select=-c(registered, count, datetime)))


## find best subset data matrix
library(caret)
set.seed(1)
k.cv = 5


val.errors=matrix(NA,p,k.cv)
folds <- createFolds(c(1:n), k=k.cv, list=TRUE, returnTrain=FALSE)
for(i in 1:p){
  coefi=coef(regfit, id=i)
  train.mat=model.mat[,names(coefi)]
  for(j in 1:k.cv){
    test=folds[[j]]
    lm.fit=lm(casual[-test]~.-1, data=data.frame(train.mat[-test,]))
    pred=as.matrix(train.mat[test,])%*%matrix(lm.fit$coef)
    val.errors[i,j]=mean((as.matrix(casual[test])-pred)^2)
  }
}
regfit.mse=apply(val.errors,1,mean)
which.min(regfit.mse)
## The best model is the one with 10 coefficients
plot(regfit.mse)
coefi=coef(regfit, id=10)
bestfit.mat=model.mat[,names(coefi)]
lm.bestfit=lm(casual~.-1,data=data.frame(bestfit.mat))
summary(lm.bestfit)
## R^2 and Adjust R^2 are both 68%, which 



## ANOVA
lm.fit1=lm(casual~poly(hour,1), data=Bike)
lm.fit2=lm(casual~poly(hour,2), data=Bike)
lm.fit3=lm(casual~poly(hour,3), data=Bike)
lm.fit4=lm(casual~poly(hour,4), data=Bike)
lm.fit5=lm(casual~poly(hour,5), data=Bike)
lm.fit6=lm(casual~poly(hour,6), data=Bike)
lm.fit7=lm(casual~poly(hour,7), data=Bike)
lm.fit8=lm(casual~poly(hour,8), data=Bike)

anova(lm.fit1, lm.fit2, lm.fit3, lm.fit4, lm.fit5, lm.fit6, lm.fit7, lm.fit8)

plt=as.matrix(cbind(rep(1,n),poly(hour,6)))%*%as.matrix(lm.fit6$coef)
par(mfrow=c(2,1))
plot(hour,plt)
plot(hour, casual)


## USING SPLINES
library(splines)
par(mfrow=c(1,1))
plot(temp,casual)
templims=range(temp)
temp.grid=seq(from=templims[1],to=templims[2])
fit.s=smooth.spline(temp, casual, cv=TRUE)
pred=predict(fit.s, newdata=list(temp=temp.grid),se=T)
fit.s$df
res=(fit.s$yin-fit.s$y)/(1-fit.s$lev)
sigma=sqrt(var(res))
upper=fit.s$y+2.0*sigma*sqrt(fit.s$lev)
lower=fit.s$y-2.0*sigma*sqrt(fit.s$lev)

plot(temp,casual)
lines(pred$y, lwd=2, col='green')
lines(upper, lwd=2, col='grey', lty=2)
lines(lower, lwd=2, col='grey', lty=2)
