---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

This assignment presents an exploratory analysis of data that was collected from a personal activity monitoring device. The data pertains to an anonymous individual and was collected during the months of October and November, 2012. The data includes the number of steps taken in 5 minute intervals each day.

I will present a number of plots and summary statistics to show the average number of steps taken per day and the average daily activity pattern. I will also impute missing values in the data and show how activity patterns vary between week days and weekends.

First, I need to ensure that all the code for this assignment is echoed in the document so that readers can be able to scrutenise the code as they read through the document. To do this I will set **echo = TRUE** as a global parameter. I will also remove the comment simbol from the results.


```r
opts_chunk$set(echo = TRUE)
opts_chunk$set(comment = NA)
```

- - -

## Loading and preprocessing the data

Here, I will unzip the data file, load it into R, and reformat some of the variables to produce tidy, analytical data.  I will also cache this process to avoid file duplication issues when ever I run the document.


```r
# Unzip data file
unzip("activity.zip", exdir = "activity")
# Load Data to R
data <- read.csv("activity/activity.csv")
# convert steps, date and interval to numeric, date and factor variables respectively
data$steps <- as.numeric(data$steps)
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- as.factor(data$interval)
```

- - -

## What is mean total number of steps taken per day?

First, we generate a data set containing the total number of steps taken per day.


```r
library(dplyr)
dateSum <- data %>%
group_by(date) %>%
summarise(total_steps = sum(steps))
```

```
`summarise()` ungrouping output (override with `.groups` argument)
```

Next, we construct a histogram of the number of steps taken each day.


```r
library(ggplot2)
ggplot(dateSum, aes(x = total_steps, na.rm = TRUE)) +
geom_histogram(bins = 300) +
labs(title = "Daily Number of Steps Taken",
subtitle = "October and November, 2012",
tag = "Figure 1",
x = "Number of steps taken")
```

```
Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->

In the last part for this section, I calculate and report the mean and median of the number of steps taken per day.


```r
stepSummary <- list(Mean = mean(dateSum$total_steps, na.rm = TRUE), Median = median(dateSum$total_steps, na.rm = TRUE)) 
print(stepSummary)
```

```
$Mean
[1] 10766.19

$Median
[1] 10765
```

- - -

## What is the average daily activity pattern?

First, I will construct a data frame containing the intervals and the steps averaged across all days.


```r
stepsInterval <- data %>%
group_by(interval) %>%
summarise(avg_steps = mean(steps, na.rm = TRUE))
```

```
`summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Convert interval to numeric vector
stepsInterval$interval <- as.numeric(stepsInterval$interval)
```

Next, I will generate a time series plot of interval against average number of steps taken.


```r
ggplot(stepsInterval, aes(x = interval, y = avg_steps)) +
geom_line(lwd = 2) +
labs(title = "Average Number of Steps Taken Per Interval",
subtitle = "October and November, 2012",
tag = "Figure 2",
x = "Interval",
y = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/Time_Series_Plot-1.png)<!-- -->

Then I will show the five minute interval that contains the highest number of steps on average across all the days.


```r
which.max(stepsInterval$avg_steps)
```

```
[1] 104
```

```r
# Find maximum number of steps in that interval
stepsInterval[which.max(stepsInterval$avg_steps), 2]
```

```
# A tibble: 1 x 1
  avg_steps
      <dbl>
1      206.
```

Therefore, the 104th interval has the maximum average dayly number of steps equaling 206 steps per day.

- - -

## Imputing missing values

First, I will assess the mechanism of missingness in the data.


```r
colSums(is.na(data))
```

```
   steps     date interval 
    2304        0        0 
```

We find that only the *steps* variable is missing values, with a total of 2304 values affected.

I choose to replace these missing values with the median value across all days for it's interval, given that the median is robust to outliers.


```r
for(i in 1:length(data$steps)) {
if(is.na(data$steps[i])) {
dat <- data %>%
filter(interval == interval[i]) %>%
summarise(med = median(steps, na.rm = TRUE))
dat <- as.numeric(dat)
data$steps[i] <- dat
}
i <- i +1
}
```

To construct the histogram, I first generate a data set containing the total number of steps taken per day.


```r
dateSum <- data %>%
group_by(date) %>%
summarise(total_steps = sum(steps))
```

```
`summarise()` ungrouping output (override with `.groups` argument)
```

Next, we construct a histogram of the number of steps taken each day.


```r
ggplot(dateSum, aes(x = total_steps, na.rm = TRUE)) +
geom_histogram(bins = 300) +
labs(title = "Daily Number of Steps Taken with imputed data",
subtitle = "October and November, 2012",
tag = "Figure 3",
x = "Number of steps taken")
```

![](PA1_template_files/figure-html/Histogram_imputed-1.png)<!-- -->

In the last part for this section, I calculate and report the mean and median of the number of steps taken per day.


```r
stepSummary <- list(Mean = mean(dateSum$total_steps, na.rm = TRUE), Median = median(dateSum$total_steps, na.rm = TRUE)) 
print(stepSummary)
```

```
$Mean
[1] 9503.869

$Median
[1] 10395
```

We notice that for the imputed data, both the mean and median are less that those for the dataframe with missing values. Overall, we can observe that total daily number of steps is slidely less for the imputed data.

- - -

## Are there differences in activity patterns between weekdays and weekends?

I will first create a factor variable in the imputed data set with two levels: "week day" and "weekend".


```r
data <- data %>%
mutate(weekDay = weekdays(date)) %>%
mutate(dayType = ifelse(weekDay == "Saturday" | weekDay == "Sunday", "Weekend", "Week Day"))
```

Next, I will construct a data frame containing the intervals and the steps averaged across weekdays and weekends.


```r
stepsInterval <- data %>%
group_by(interval, dayType) %>%
summarise(avg_steps = mean(steps, na.rm = TRUE))
```

```
`summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
# Convert interval to numeric vector
stepsInterval$interval <- as.numeric(stepsInterval$interval)
```

Next, I will generate a time series plot of interval against average number of steps taken by type of day.


```r
ggplot(stepsInterval, aes(x = interval, y = avg_steps)) +
geom_line(lwd = 2) +
facet_grid(. ~ dayType) +
labs(title = "Average Number of Steps Taken Per Interval by Day Type",
subtitle = "October and November, 2012",
tag = "Figure 4",
x = "Interval",
y = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/Time_Series_Plot_Day-1.png)<!-- -->

- - -

