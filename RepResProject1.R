# Reproducible Research
# Project 1

data <- read.csv("activity.csv", stringsAsFactor = F)
data$date <- as.Date(data$date)

dailySteps <- aggregate(data$steps, list(date = data$date), sum)

library(ggplot2)
qplot(dailySteps$x, binwidth=2500, xlab="Daily Steps", ylab="Count")

dailyStepsMean <- mean(dailySteps$x, na.rm=T)
dailyStepsMedian <- median(dailySteps$x, na.rm=T)

numNA <- sum(is.na(data$steps))
naRows <- which(is.na(data$steps))

dataNoNA <- data

for (row in naRows){
    interval <- dataNoNA[row,]$interval
    newVal <- mean(data[data$interval == interval,]$steps, na.rm = TRUE)
    dataNoNA[row,]$steps <- newVal
}

dailyStepsNoNA <- aggregate(dataNoNA$steps, list(date = dataNoNA$date), sum)
qplot(dailyStepsNoNA$x, binwidth=2500, xlab="Daily Steps", ylab="Count")

dailyStepsNoNAMean <- mean(dailyStepsNoNA$x)
dailyStepsNoNAMedian <- median(dailyStepsNoNA$x)

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dayCheck <- sapply(dataNoNA$date, weekdays)
dayType <- dayCheck %in% weekdays
dayType[dayType == TRUE] <- "Weekday"
dayType[dayType == FALSE] <- "Weekend"
dataNoNA$dayType <- as.factor(dayType)
rm(weekdays, dayCheck)

avgDaily <- aggregate(dataNoNA$steps, list(Day = dataNoNA$dayType, Interval = dataNoNA$interval), mean)
qplot(Interval, x, facets=.~Day, data=avgDaily, geom="line", ylab="Steps")
