---
output: 
  html_document: 
    fig_caption: yes
    
    keep_md: yes
    

---
#Course Project 1 - Reproducible Research

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Questions to be answered:

 - What is mean total number of steps taken per day?
 - What is the average daily activity pattern?
 - Imputing missing values
 - Are there differences in activity patterns between weekdays and weekends?
 
##Loading and preprocessing the data:
I downloaded the data of the analysis from [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

```r
#reading the data
data <- read.csv("./activity.csv")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#transform date column into date format
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
data$date <-ymd(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##What is mean total number of steps taken per day?

```r
#creating the histogram 
total_steps <- with(data,aggregate(steps, by= list(date), FUN = sum, na.rm = TRUE))
names(total_steps) <- c("date", "steps")
hist(total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps per day", col = "red",breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
###Mean of total number of steps
mean(total_steps$steps)
```

```
## [1] 9354.23
```

```r
###Median of total number of steps
median(total_steps$steps)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

```r
average_steps <- with(data, aggregate(steps, by = list(interval), FUN = mean, na.rm = TRUE))
names(average_steps) <- c("interval","mean")
plot(average_steps$interval, average_steps$mean, type = "l", col = "blue", xlab = "5-minute interval", ylab = "average number of steps", main = "average number of steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_steps[which.max(average_steps$mean), ]$interval
```

```
## [1] 835
```

##Imputing missing values

```r
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(data[1]))
```

```
## [1] 2304
```

```r
###Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
imputed <- average_steps$mean[match(data$interval, average_steps$interval)]

###Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputed_data <- transform(data, steps= ifelse(is.na(data$steps), yes = imputed, no = data$steps))

###Make a histogram of the total number of steps taken each day
total_steps_imputed <- aggregate(steps ~ date, imputed_data, sum)
names(total_steps_imputed) <- c("date" , "steps_by_day")

hist(total_steps_imputed$steps_by_day, col = "red",breaks = seq(0,25000, by=2500), main = "Total number of steps per day", xlab = "Total number of steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
###Mean of the total number of steps taken per day
mean(total_steps_imputed$steps_by_day)
```

```
## [1] 10766.19
```

```r
###Median of the total number of steps taken per day
median(total_steps_imputed$steps_by_day)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

```r
###Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
###setting my system time to English
Sys.setlocale("LC_ALL","English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
data$date_type <- sapply(data$date, function(x){
    if(weekdays(x) == "Saturday" | weekdays(x)== "Sunday"){
        y = "weekend"
    }else {
        y = "weekday"
    }
    y
})
data$date_type <- as.factor(data$date_type)

###Make a panel plot containing a time series plot (i.e. type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
library(ggplot2)
data_by_date <- aggregate(steps~interval + date_type, data, mean, na.rm =TRUE)
g <- ggplot(data_by_date, aes(x = interval, y = steps, color = date_type)) + labs(title = "Average daily steps by date types", x = "Interval", y = "average number of steps" ) +  geom_line() + facet_wrap(~date_type, ncol = 1, nrow = 2)
g 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
