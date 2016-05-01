# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# read in the file
activity <- read.csv("activity.csv")
# convert data column to Date type
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
# create a new dataset with non-NA steps excluded
nonNaActivity <- activity[!is.na(activity$steps), ]

# aggregrate, total steps by date
totalStepsPerDaySet <- aggregate(list(total = nonNaActivity$steps), FUN = sum, by = list(Group.date = nonNaActivity$date))

# create a histogram
hist(totalStepsPerDaySet$total, xlab = "", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
# Calculate and report the mean and median total number of steps taken per day
cat("Mean of total steps  per day:", mean(totalStepsPerDaySet$total))
```

```
## Mean of total steps  per day: 10766.19
```

```r
cat("Median of total steps per day:", median(totalStepsPerDaySet$total))
```

```
## Median of total steps per day: 10765
```


## What is the average daily activity pattern?

```r
# aggregrate steps over intervals
avgStepsByInterval <- aggregate(steps ~ interval, data = nonNaActivity, mean)

# plot the same
plot(avgStepsByInterval$interval, avgStepsByInterval$steps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
# find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps 
print(avgStepsByInterval[avgStepsByInterval$steps == max(avgStepsByInterval$steps), ]$interval)
```

```
## [1] 835
```


## Imputing missing values

```r
# report the total number of missing values in the dataset
print(sum(is.na(activity$steps)))
```

```
## [1] 2304
```

```r
# let's use mean for that 5-minute interval
library(dplyr)
activityImputed <- activity %>% group_by(interval) %>% mutate(steps= replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))

#calculate total Steps/day from this new dataset
totalStepsPerDayImputedSet <- aggregate(list(total = activityImputed$steps), FUN = sum, by = list(Group.date = activityImputed$date))

# make a histogram
hist(totalStepsPerDayImputedSet$total, xlab = "", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
# report mean and median of imputed data set
cat("Mean of total steps  per day:", mean(totalStepsPerDayImputedSet$total))
```

```
## Mean of total steps  per day: 10766.19
```

```r
cat("Median of total steps per day:", median(totalStepsPerDayImputedSet$total))
```

```
## Median of total steps per day: 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
weekdaysList <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# add a factor column for weekday/weekend
activityImputed$typeOfDay <- factor(weekdays(activityImputed$date) %in% weekdaysList, levels = c(FALSE, TRUE), labels=c("weekend", "weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
# aggregrate steps by interval, as well as day type
avgStepsByIntervalAndDayType <- aggregate(steps ~ interval + typeOfDay, activityImputed, mean)

#plot the same
library(lattice)
xyplot(steps ~ interval | typeOfDay, data = avgStepsByIntervalAndDayType, type = "l", xlab = "5-minute interval", ylab = "Average number of steps", layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)
