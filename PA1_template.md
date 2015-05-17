---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

  
## Loading and preprocessing the data

Unzip the downloaded file and load into R Studio. Convert date into a date object.


```r
setwd("~/datasciencecoursera/RepData_PeerAssessment1")  ## location of zip file
unzip(zipfile="activity.zip")
library(ggplot2)

activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```

  
## What is mean total number of steps taken per day?

For this part of the assignment we ignore the missing values in the dataset and
calculate total number of steps taken for each day we have measurements.


```r
stepsPerDay <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
```

Now let us build a histogram to visualize total steps taken by each day.


```r
qplot(stepsPerDay, binwidth=600, xlab="Total number of steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Now calculate mean and median value of steps taken per day.


```r
mean(stepsPerDay)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay)
```

```
## [1] 10395
```

  
## What is the average daily activity pattern?


```r
avg <- aggregate(x=list(steps=activity$steps),
        by=list(interval=activity$interval),
        FUN=mean, na.rm=TRUE)

ggplot(data=avg, aes(x=interval, y=steps)) + geom_line() +
   xlab("5-minute interval") +
   ylab("Average number of steps taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

  
## Imputing missing values

Calculate the total number of rows with NA or missing values is shown here.

```r
length(activity$steps[is.na(activity$steps)])
```

```
## [1] 2304
```

When we have missing values, we use mean for that day's steps average (by
interval) to substitue for missing values.  


```r
filler <- function(steps, interval) {
     if (!is.na(steps))
          val <- c(steps)
     else
          val <- avg[avg$interval==interval, "steps"]

     return(val)
}

complData <- activity
complData$steps <- mapply(filler, complData$steps, complData$interval)
```

Then we make a histogram of the total number of steps taken each day and 
calculate to report the mean and median total number of steps taken per day.  


```r
complSteps <- tapply(complData$steps, complData$date, FUN=sum)
qplot(complSteps, binwidth=600, xlab="Total number of steps per day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(complSteps)
```

```
## [1] 10766.19
```

```r
median(complSteps)
```

```
## [1] 10766.19
```

The values of mean and median are higher compared to earlier values where missing
values removed from calculation. By imputing missing data values both mean
and median are reporting the same value as you see above.  


## Are there differences in activity patterns between weekdays and weekends?

First figure out whether a day is weekend or weekday. Add a column to the data
frame.


```r
weekdayOrWeekend <- function(date) {
      day <- weekdays(date)
      if (day %in% c("Saturday", "Sunday"))
          return("weekend")
      else
          return("weekday") 
}

complData$day <- sapply(complData$date, FUN=weekdayOrWeekend)
```

Now let us panel plot separately for weekdays and weekend and compare.  


```r
avg2 <- aggregate(steps ~ interval + day, data=complData, mean)
ggplot(avg2, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
   xlab("5-min interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Based on comparing above weekend activity is more distributed throughout the
day (of awake hours) whereas for weekday it is very high in early morning hours. 
