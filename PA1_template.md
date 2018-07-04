---
title: "RepData_PeerAssessment1"
output: html_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading the data
Load the data
``` {r}
task=read.csv("./activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
num_Steps<-aggregate(steps~date,data=task,sum,na.rm=TRUE)

```

## What is mean total number of steps taken per day?
Plot a histogram of the total number of steps taken each day
```{r}
hist(num_Steps$steps)
```
Calculate the mean and median total number of steps taken per day
```{r}
mean(num_Steps$steps)

```

```{r}
median(num_Steps$steps)
```

## What is the average daily activity pattern?
Plot time series graph (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
steps_Interval<-aggregate(steps~interval,data=task,mean,na.rm=TRUE)
plot(steps~interval,data=steps_Interval,type="l")
```

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}

steps_Interval[which.max(steps_Interval$steps),]$interval
```


## Imputing missing values
Calculate  the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(task$steps))
```


```{r}
sum(is.na(task$steps)) 
```


Using mean to impute 5-minute interval. 

```{r}
interval_steps<-function(interval){
    steps_Interval[steps_Interval$interval==interval,]$steps
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activityFilled<-task   # Make a new dataset with the original data
count=0           # Count the number of data filled in
for(i in 1:nrow(activityFilled)){
    if(is.na(activityFilled[i,]$steps)){
        activityFilled[i,]$steps<-interval_steps(activityFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values were filled.\n\r")  
```

Plot a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r}
totalSteps2<-aggregate(steps~date,data=activityFilled,sum)
hist(totalSteps2$steps)
```

```{r}
mean(totalSteps2$steps)
```

```{r}
median(totalSteps2$steps)
```

## Are there differences in activity patterns between weekdays and weekends?


```{r}
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend","weekday")
# For Sunday and Saturday : weekend, Other days : weekday 

activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))
```

Plot a panel containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}
steps_Interval2=aggregate(steps~interval+day,activityFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=steps_Interval2,aspect=1/2,type="l")
```
