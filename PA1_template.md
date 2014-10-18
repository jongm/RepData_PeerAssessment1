---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

1. Load the data:

```r
data <- read.csv("activity.csv")
```

2. Process/transform the data:

```r
time <- strptime(data$date, "%Y-%m-%d")
data$day <- time$yday
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day:

```r
totals <- aggregate(data$steps, by=list(data$day), FUN=sum, na.rm=TRUE)
names(totals) <- c("day","total_steps")
hist(totals$total_steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day:

```r
mean(totals$total_steps)
```

```
## [1] 9354.23
```

```r
median(totals$total_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
means <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
names(means) <- c("interval","mean_steps")
plot(means$mean_steps ~ means$interval, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
means$interval[which.max(means$mean_steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2 and 3. Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in:

```r
newdata <- data
for (i in 1:nrow(newdata)) {
  if (is.na(newdata$steps[i])) {
  newdata$steps[i] <- means$mean_steps[means$interval==newdata$interval[i]]}
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newtotals <- aggregate(newdata$steps, by=list(newdata$day), FUN=sum)
names(newtotals) <- c("day","total_steps")
hist(newtotals$total_steps)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(newtotals$total_steps)
```

```
## [1] 10766.19
```

```r
median(newtotals$total_steps)
```

```
## [1] 10766.19
```

Values are now higher than before because previously we had NAs (since they were not added they were acting as zeroes for aggregate) while now we have positive numbers.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day:

```r
newdata$daytype <- NA
for (i in 1:nrow(newdata)) {
  if (weekdays(time[i]) %in% c("sábado", "domingo")) {
    newdata$daytype[i] <- "weekend"
  }
  else {
    newdata$daytype[i] <- "weekday"
  }
}
newdata$daytype <- as.factor(newdata$daytype)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```r
meanswd <- aggregate(newdata$steps[newdata$daytype=="weekday"], by=list(newdata$interval[newdata$daytype=="weekday"]), FUN=mean)
names(meanswd) <- c("interval","mean_steps")
meanswe <- aggregate(newdata$steps[newdata$daytype=="weekend"], by=list(newdata$interval[newdata$daytype=="weekend"]), FUN=mean)
names(meanswe) <- c("interval","mean_steps")

par(mfrow=c(2,1))
plot(meanswe$mean_steps ~ meanswe$interval, type="l", main="WEEKENDS", col="blue", xlab="", ylab="")
plot(meanswd$mean_steps ~ meanswd$interval, type="l", main="WEEKDAYS", col="blue", xlab="", ylab="")
mtext("Number of steps", 2, padj=-3, adj=-48)
mtext("Interval", 1, padj=4)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
