---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: yes
---


## Loading and preprocessing the data

### Load the data

Data loaded into **activity** tibble using **tidyverse::read_csv()** 


```r
library(tidyverse)
activity <- read_csv("activity.zip")
```

## What is mean total number of steps taken per day?

### Histogram of total number of steps taken per day


```r
activity %>% drop_na(steps) %>%
      group_by(date) %>% 
      summarise(daily_steps = sum(steps, na.rm = T)) %>% 
      ggplot(aes(x = daily_steps)) +
      geom_histogram(binwidth = 2000,
                     fill = "steelblue",
                     colour = "white") +
      labs(title = "Daily steps histogram",
           x = "Daily steps",
           y = "") +
      theme_minimal()
```

![](PA1_template_files/figure-html/stepshist-1.png)<!-- -->

### Mean and median of total number of steps taken per day


```r
daily_steps <- activity %>% drop_na(steps) %>%
      group_by(date) %>% 
      summarise(steps = sum(steps, na.rm = T))
options(scipen = 999999)
daily_steps_mean <- mean(daily_steps$steps)
daily_steps_median <- median(daily_steps$steps)
```

The mean of total number of steps taken per day is **10766.19** and the median is **10765**.


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
