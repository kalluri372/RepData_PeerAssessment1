---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

In this report, we analyze the data from an activity monitor on a single 
individual over a period of two months, during October and November 2012. The 
data consists of the number of steps taken during each 5 minute interval of each 
day during this period. In the following, we read the data from a CSV file, and 
each subsequent section answers the specific question indicated by the section 
name.

## Loading and preprocessing the data

As the code below shows, the data consists of three columns: "steps", "date" 
and "interval". 


```r
system("cat activity.csv | head -6", intern = TRUE)
```

```
## character(0)
```

We read the CSV file into a data frame, and in order to identify patterns on a
per-interval, per-day and weekend-vs-weekday basis, we augment the data frame 
with additional factors as columns. Summary properties of the augmented data 
frame are also shown below. The data consists of 17568 = (12 5-minute intervals 
per hour)\*(24 hours per day)\*(61 days). The date is converted to a factor 
"day_f" counted from the first date, thus ranging from 1 to 61. The 5-minute 
interval identifier, ranging for each day from 0 to 2355 (e.g. "2355" stands for 
23 hours and 55 minutes), is converted to a factor "interval_f". 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity_df <- read.csv( file = "activity.csv", 
                         colClasses = c("numeric", "Date", "integer"),
                         nrows = 17568 )
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
activity_df <- 
 activity_df %>% 
 mutate( interval_f = factor(interval), 
         day_f = factor(date - date[1] + 1) )
```

```
## Error in eval(expr, envir, enclos): object 'activity_df' not found
```

```r
str(activity_df)
```

```
## Error in str(activity_df): object 'activity_df' not found
```

```r
summary(activity_df)
```

```
## Error in summary(activity_df): object 'activity_df' not found
```

## What is mean total number of steps taken per day?

First, we compute the total number of steps per day (ignoring missing values) 
for each of the 61 days, and plot its histogram. 


```r
per_day <- 
 activity_df %>% 
 group_by( day_f ) %>% 
 summarize( steps_per_day = sum(steps, na.rm = TRUE) )
```

```
## Error in eval(expr, envir, enclos): object 'activity_df' not found
```

```r
per_day_steps <- per_day$steps_per_day
```

```
## Error in eval(expr, envir, enclos): object 'per_day' not found
```

```r
hist( per_day_steps, col = "blue", breaks = 10, 
      main = "histogram of total steps per day" )
```

```
## Error in hist(per_day_steps, col = "blue", breaks = 10, main = "histogram of total steps per day"): object 'per_day_steps' not found
```

```r
rug( per_day$steps_per_day )
```

```
## Error in as.vector(x): object 'per_day' not found
```

Then, we determine the mean and median of the total number of steps per day. 


```r
print(  mean( per_day$steps_per_day )  )
```

```
## Error in mean(per_day$steps_per_day): object 'per_day' not found
```

```r
print(  median( per_day$steps_per_day )  )
```

```
## Error in median(per_day$steps_per_day): object 'per_day' not found
```

## What is the average daily activity pattern?

First, we compute the average (across all days) of the number of steps per daily
interval (ignoring missing values) and plot it as a time series across the day. 


```r
per_interval <- 
 activity_df %>% 
 group_by( interval_f ) %>% 
 summarize( avg_steps_per_interval = mean(steps, na.rm = TRUE) )
```

```
## Error in eval(expr, envir, enclos): object 'activity_df' not found
```

```r
str(per_interval)
```

```
## Error in str(per_interval): object 'per_interval' not found
```

```r
intervals <- as.integer(per_interval$interval_f)
```

```
## Error in eval(expr, envir, enclos): object 'per_interval' not found
```

```r
avg_per_interval_steps <- per_interval$avg_steps_per_interval
```

```
## Error in eval(expr, envir, enclos): object 'per_interval' not found
```

```r
plot( intervals, avg_per_interval_steps, type = "l", col = "red", 
      xlab = "interval", ylab = "average number of steps",  
      main = "across-all-days average of steps per interval" )
```

```
## Error in plot(intervals, avg_per_interval_steps, type = "l", col = "red", : object 'intervals' not found
```

Then, we determine the interval with the maximum across-61-days average number 
of steps, printing the maximizing index (which is in 1-288) first and then the 
corresponding interval identifier. 


```r
print(  which.max( avg_per_interval_steps )  )
```

```
## Error in which.max(avg_per_interval_steps): object 'avg_per_interval_steps' not found
```

```r
print(  per_interval$interval_f[ which.max( avg_per_interval_steps ) ]  )
```

```
## Error in print(per_interval$interval_f[which.max(avg_per_interval_steps)]): object 'per_interval' not found
```

The maximizing interval is the 104th, out of the 288 intervals in a day, and 
corresponds to the 5-minute interval with identifier "835", i.e. ending 8:35am.

## Imputing missing values

The indexes of rows in the data with missing values of "steps" are determined 
below, along with the total number of such rows. 


```r
activity_df <- 
 activity_df %>% 
 mutate( is_missing_steps = is.na(steps) )
```

```
## Error in eval(expr, envir, enclos): object 'activity_df' not found
```

```r
indexes_missing_steps <- which( activity_df$is_missing_steps )
```

```
## Error in which(activity_df$is_missing_steps): object 'activity_df' not found
```

```r
print( length(indexes_missing_steps) )
```

```
## Error in print(length(indexes_missing_steps)): object 'indexes_missing_steps' not found
```

As confirmation, note that we already found this total number of NAs when we 
applied the summary() function in the "loading and preprocessing" section.

We shall fill in the missing value of a particular interval (of a particular 
day) with the previously-computed across-all-days average for that interval of 
the day, which we had saved in the vector "avg_per_interval_steps", with the 
corresponding interval identifiers found in "per_interval$interval_f".

The summaries of the resulting data frame "filled_activity_df", and a look at 
the top few values, demonstrate that we have cleaned up the missing values. 


```r
filled_activity_df <- 
 activity_df %>% 
 mutate( steps = ifelse( is_missing_steps, 
                         avg_per_interval_steps[ match(interval_f, 
                                                       per_interval$interval_f) ], 
                         steps
                       ) 
       )
```

```
## Error in eval(expr, envir, enclos): object 'activity_df' not found
```

```r
summary( filled_activity_df )
```

```
## Error in summary(filled_activity_df): object 'filled_activity_df' not found
```

```r
str( filled_activity_df )
```

```
## Error in str(filled_activity_df): object 'filled_activity_df' not found
```

```r
temp_head_df <- head( filled_activity_df[ is.na(activity_df$steps), ] )
```

```
## Error in head(filled_activity_df[is.na(activity_df$steps), ]): object 'filled_activity_df' not found
```

```r
temp_head_df
```

```
## Error in eval(expr, envir, enclos): object 'temp_head_df' not found
```

```r
avg_per_interval_steps[ match( temp_head_df$interval_f, 
                               per_interval$interval_f ) ]
```

```
## Error in eval(expr, envir, enclos): object 'avg_per_interval_steps' not found
```

The following is a repeat of the section "what is mean total number of steps 
taken per day", but applied to the missing-values-filled dataset.

First, we compute the total number of steps per day for each of the 61 days, 
and plot its histogram. 


```r
filled_per_day <- 
 filled_activity_df %>% 
 group_by( day_f ) %>% 
 summarize( steps_per_day = sum(steps, na.rm = TRUE) )
```

```
## Error in eval(expr, envir, enclos): object 'filled_activity_df' not found
```

```r
filled_per_day_steps <- filled_per_day$steps_per_day
```

```
## Error in eval(expr, envir, enclos): object 'filled_per_day' not found
```

```r
hist( filled_per_day_steps, col = "magenta", breaks = 10, 
      main = "histogram of total steps per day" )
```

```
## Error in hist(filled_per_day_steps, col = "magenta", breaks = 10, main = "histogram of total steps per day"): object 'filled_per_day_steps' not found
```

```r
rug( filled_per_day$steps_per_day )
```

```
## Error in as.vector(x): object 'filled_per_day' not found
```

Then, we determine the mean and median of the total number of steps per day. 


```r
print(  mean( filled_per_day$steps_per_day )  )
```

```
## Error in mean(filled_per_day$steps_per_day): object 'filled_per_day' not found
```

```r
print(  median( filled_per_day$steps_per_day )  )
```

```
## Error in median(filled_per_day$steps_per_day): object 'filled_per_day' not found
```


Finally, we observe that missing-value imputation concentrates the distribution 
around its mean/median (although it is unclear why the new mean and median are 
*exactly* the same in the results above, rather than merely closer than they were
before).

## Are there differences in activity patterns between weekdays and weekends?

We first augment the filled-in data frame with a factor variable indicating if 
the date was on a weekday vs. on a weekend. 


```r
filled_activity_df <- 
 filled_activity_df %>% 
 mutate( daytype_f = factor( ifelse(weekdays(date) %in% c("Saturday", "Sunday"), 
                                     "weekend", "weekday"), 
                             levels = c("weekday", "weekend") 
                           )
       )
```

```
## Error in eval(expr, envir, enclos): object 'filled_activity_df' not found
```

```r
table( filled_activity_df$daytype_f )
```

```
## Error in table(filled_activity_df$daytype_f): object 'filled_activity_df' not found
```

Finally, we compare the activity patterns on weekdays vs. weekends, by plotting 
the across-all-weekdays-average per-interval number of steps and the across-all-weekends-average per-interval number of steps, in a panel plot with two panels.


```r
per_daytype_per_interval <- 
 filled_activity_df %>% 
 group_by( daytype_f, interval_f ) %>% 
 summarize( avg_per_daytype_steps_per_interval = mean(steps, na.rm = TRUE) )
```

```
## Error in eval(expr, envir, enclos): object 'filled_activity_df' not found
```

```r
str(per_daytype_per_interval)
```

```
## Error in str(per_daytype_per_interval): object 'per_daytype_per_interval' not found
```

```r
f_day_type <- per_daytype_per_interval$daytype_f
```

```
## Error in eval(expr, envir, enclos): object 'per_daytype_per_interval' not found
```

```r
intervals2 <- as.integer(per_daytype_per_interval$interval_f)
```

```
## Error in eval(expr, envir, enclos): object 'per_daytype_per_interval' not found
```

```r
avg_per_daytype_per_interval_steps <- 
 per_daytype_per_interval$avg_per_daytype_steps_per_interval
```

```
## Error in eval(expr, envir, enclos): object 'per_daytype_per_interval' not found
```

```r
library(lattice)
xyplot( avg_per_daytype_per_interval_steps ~ intervals2 | f_day_type, 
        panel = function(x, y, ...) { 
                        panel.xyplot(x, y, ..., 
                                     type = "l") 
                },
        xlab = "interval", 
        ylab = "Number of steps", 
        layout = c(1, 2), 
        col = "blue", 
        main = "across-all-days average of steps per interval" 
      )
```

```
## Error in eval(expr, envir, enclos): object 'f_day_type' not found
```

We see that the 2-month average number of steps has a more distinct peak (around
get-to-work time) for weekdays, when compared to weekends. Further, there is a
reduction in activity over the weekend (peak > 200 average-number-of-steps for
weekdays, versus ~ 150 steps for weekends).

The plot is also saved into the "./figure/" directory as a PNG file.


```r
dev.copy( png, file = "./figure/weekday_weekend_panelplot.png" )
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

