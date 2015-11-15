# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First, we are going to unzip and read the data. Then, we convert date variable into Date format.


```r
if (!file.exists("activity.csv"))
    unzip("activity.zip")

data <- read.csv("activity.csv")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
# Pre-processed data
data.prep <- data
rm(data)

# Reformat date variable
data.prep$date <- as.Date(data.prep$date)
str(data.prep)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data.prep)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
# convert to tbl_df to use dplyr functions
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data.prep <- tbl_df(data.prep)
```

## What is mean total number of steps taken per day?

First, we calculate the total number of steps taken per day.

```r
total.steps <- group_by(data.prep, date) %>%
    summarize(steps= sum(steps, na.rm=T))
head(total.steps)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
##       (date) (int)
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

Next, we will plot the histogram of total steps taken per day.


```r
library(ggplot2)
ggplot(data = total.steps, aes(x= steps)) +
    geom_histogram(fill= "Blue", color="Black", binwidth= 1000) +
    coord_cartesian(ylim = c(0, 12))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Next, we calculate **mean** and **median** steps taken per day.


```r
mean(total.steps$steps)
```

```
## [1] 9354.23
```

```r
median(total.steps$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

First, let's figure out the average number of steps taken during each interval.


```r
interval.steps <- group_by(data.prep, interval) %>%
    summarize(mean= mean(steps, na.rm= T))

ggplot(interval.steps, aes(x= interval, y= mean)) +
           geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

We would like to know which interval has the maximum number of steps taken, on average.

```r
max.interval <- interval.steps[which.max(interval.steps$mean),]

max.interval.time <- as.POSIXct("00:00:00", format="%H:%M:%S")
max.interval.time <- max.interval.time + max.interval$interval
max.interval.time <- as.POSIXlt(max.interval.time)
strftime(max.interval.time, format="%M:%S")
```

```
## [1] "13:55"
```

```r
max.interval
```

```
## Source: local data frame [1 x 2]
## 
##   interval     mean
##      (int)    (dbl)
## 1      835 206.1698
```

On average, the interval with the most activity is at 13:55 everyday, with 206 steps.

## Imputing missing values

First, let's see how many rows have missing values.


```r
summary(data.prep)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

As we can see in the results of *summary()* function, **data** column and **interval** column don't have any missing values, but **steps** column has 2304 missing values.

To impute the missing values, we first need to determine the nature of the missingness.


```r
daily.missing.steps <- group_by(data.prep, date) %>%
    summarize(nas= sum(is.na(steps)))

ggplot(daily.missing.steps, aes(x= date, y= nas)) +
    geom_bar(stat = "identity", fill= "blue", color= "black")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
interval.missing.steps <- group_by(data.prep, interval) %>%
    summarize(nas= sum(is.na(steps)))
    
ggplot(interval.missing.steps, aes(x= interval, y= nas)) +
    geom_bar(stat = "identity", fill= "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-2.png) 

Since the missing values are concentrated in some specific dates, and since the activity levels tend to be different during different times of the day, I would go with the **mean** activity level across **all** the remaining days during that specific **interval** as the imputed values.


```r
# get mean steps count for the same interval, where steps are valid
mean.steps <- filter(data.prep, !is.na(steps)) %>%
    group_by(interval) %>%
    summarize(mean= mean(steps))

# data set to contain original and imputed values
data.imputed <- data.prep

# where are the missing values
missing <- which(is.na(data.imputed$steps))

# get mean steps count for a specific interval
get.mean <- function(x) {
    mean.steps$mean[mean.steps$interval == x]
}

data.imputed$steps[missing] <- sapply(data.imputed$interval[missing], get.mean)
```

Now, we re-create the histogram of number of steps taken per day, with imputed data.

```r
total.steps.imputed <- group_by(data.imputed, date) %>%
    summarize(steps= sum(steps))

ggplot(total.steps.imputed, aes(x= steps)) +
    geom_histogram(fill="blue", color="black", binwidth= 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Now, we can compare mean and media steps taken per day, before and after imputing data. **Before** imputing data:

```r
mean(total.steps$steps)
```

```
## [1] 9354.23
```

```r
median(total.steps$steps)
```

```
## [1] 10395
```

**After** imputing missing data:

```r
mean(total.steps.imputed$steps)
```

```
## [1] 10766.19
```

```r
median(total.steps.imputed$steps)
```

```
## [1] 10766.19
```

As the resut of imputation, both mean and median are increased, with mean increasing more.

## Are there differences in activity patterns between weekdays and weekends?

Let's take a look at difference in activity levels during weekdays vs. weekends.

```r
data.imputed <- mutate(data.imputed, weekday= factor(ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")))

ggplot(data.imputed, aes(x= interval, y=steps)) +
    stat_summary(geom="line", fun.y= mean) +
    #geom_line() +
    facet_wrap(~weekday, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

In the weekends, activities start later and have lower maximum and finish later, on average. But, in general this individual looks more active during the weekends.
