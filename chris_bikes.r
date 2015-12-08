
# install.packages('ggplot2', repos = "http://cran.us.r-project.org")
# install.packages('scales', repos = "http://cran.us.r-project.org")
# install.packages('lubridate', repos = "http://cran.us.r-project.org")
# install.packages('readr', repos = "http://cran.us.r-project.org")
# install.packages('caret', repos = "http://cran.us.r-project.org")

library(ggplot2)
library(lubridate)
library(readr)
library(scales)
library(reshape2)
library(plyr)
library(caret)

# The competition data is stored in the ../input directory
train <- read.csv("./train.csv")
test  <- read.csv("./test.csv")

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
test <- apply_transformations(test)

# Tweak these to show something else on the axes
x_axis <- "jitter_times"
y_axis <- "count"
color  <- "temp_f" # for example swap this to "humidity"


#train$jitter_times <- train$times+minutes(round(runif(nrow(train),min=0,max=59)))

head(train)

head(train)

head(test)

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

head(train)

full_dates <- train.count.by.date_hour[train.count.by.date_hour$datetime == 24,]$date

# We need to trim the ends so that there's an equal representation of the days of the week
min(train$est)
wday(ymd_hms(min(train$est)), label=TRUE)
max(train$est)
wday(ymd_hms(max(train$est)), label=TRUE)

tapply(train$count, train$day, mean)
boxplot(train$count ~ train$day)
tapply(train$count, train$workingday, mean)
boxplot(train$count ~ train$workingday)

anova.data <- ddply(train[train$est >= '2011-01-01' & train$est < '2012-12-15' ,], c("day", "date"), summarise, count = sum(count))

anova.fit <- aov(count ~ day+hour, data=train[train$est >= '2011-01-01' & train$est < '2012-12-15',])
summary(anova.fit)

anova.fit
print(model.tables(anova.fit,"means"),digits=3)
#layout(matrix(c(1,2,3,4),2,2)) # optional layout 
#plot(anova.fit)

pairwise.t.test(anova.data$count, anova.data$day, p.adjust="bonferroni")

anova.fit <- aov(count ~ day, data=train[train$est >= '2011-01-01' & train$est < '2012-12-15',])
summary(anova.fit)
print(model.tables(anova.fit,"means"),digits=3)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(anova.fit)

plot(anova.fit)

#

table(train$day)

n

f <- as.formula(count ~ (season + weather + holiday + temp + atemp + humidity + windspeed + hour + day)* days.from.start)
reg.fit <- lm(f, data=train)
summary(reg.fit)

plot(reg.fit)



set.seed(2)
k.cv = 4
folds <- createFolds(train[,1], k=k.cv, list=TRUE, returnTrain=FALSE)
mse <- c()
for (i in 1:k.cv) {
    reg.fit <- lm(f, data=train[-folds[[i]],])
    predictions <- predict(reg.fit, train[folds[[i]],])
    mse[i] <- mean((predictions - train[folds[[i]],'count'])^2)   
}
mean(mse)

predictions <- predict(reg.fit, train[folds[[i]],])





p <- ggplot(train, aes(x=times, y=count, color=day)) +
     geom_smooth(ce=FALSE, fill=NA, size=2) +
     theme_light(base_size=20) +
     xlab("Hour of the Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Number of Bike Rentals") +
     scale_color_discrete("") +
     ggtitle("People rent bikes for morning/evening commutes on weekdays, and daytime rides on weekends\n") +
     theme(plot.title=element_text(size=18))
p
#ggsave("bike_rentals_by_time_of_day.png")


train$hour  <- hour(ymd_hms(train$datetime))
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$day   <- wday(ymd_hms(train$datetime), label=TRUE)

p <- ggplot(train, aes(x=times, y=count, color=day)) +
     geom_smooth(ce=FALSE, fill=NA, size=2) +
     theme_light(base_size=20) +
     xlab("Hour of the Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Number of Bike Rentals") +
     scale_color_discrete("") +
     ggtitle("People rent bikes for morning/evening commutes on weekdays, and daytime rides on weekends\n") +
     theme(plot.title=element_text(size=18))
p
#ggsave("bike_rentals_by_time_of_day.png")


# Write some basic stats to the log
cat("Number of training rows ", nrow(train), "\n")
cat("Number of test rows ", nrow(test), "\n")
head(train)

p <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis)) +
     geom_point(position=position_jitter(w=0.0, h=0.4)) +
     theme_light(base_size=20) +
     xlab("Hour of the Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Number of Bike Rentals") +
     scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
     ggtitle("On workdays, most bikes are rented on warm mornings and evenings\n") +
     theme(plot.title=element_text(size=18))
p
#ggsave("bike_rentals_by_time_and_temperature.png", p)

p <- ggplot(train, aes_string(x=x_axis, y=y_axis, color=color)) +
     geom_point(position=position_jitter(w=0.0, h=0.4)) +
     theme_light(base_size=20) +
     xlab("Hour of the Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Number of Bike Rentals") +
     scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
     ggtitle("On workdays, most bikes are rented on warm mornings and evenings\n") +
     theme(plot.title=element_text(size=18))
p



cormat <- round(cor(train[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)]), 3)
reorder_cormat <- function(cormat){
# Use correlation between variables as distance
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)

get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }

lower_cormat <- get_upper_tri(cormat)
melted_cormat <- melt(lower_cormat, na.rm=TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed() + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(0.6, 0.7),
  legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                title.position = "top", title.hjust = 0.5))
ggheatmap
