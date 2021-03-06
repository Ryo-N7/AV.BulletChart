---
title: "Untitled"
author: "RN7"
date: "February 24, 2018"
output: 
  html_document: 
    keep_md: yes
---



## Bullet Charts for Monitoring KPIs

This is the repo for the article found [here]() on creating bullet charts for monitoring KPIs over time. The article mainly focused on `M&E` deliverables, or **"Indicators"**, but this post and repo will also come in handy for anyone that depends on Key Performance Indicators (KPI)s or those that need to track progress against a target.

Let's look at the kind of data frame you would need to create the chart:





```r
glimpse(df)
```

```
## Observations: 6
## Variables: 11
## $ IndicatorName      <chr> "Ind 04", "Ind 05", "Ind 07", "Ind 11", "In...
## $ Actual             <dbl> 3, 437, 20, 44, 1, 10000
## $ Actual_lastWeek    <dbl> 3, 420, 18, 20, 1, 10000
## $ Actual_lastYear    <dbl> 3, 50, 20, 2000, 1, 10000
## $ Target             <dbl> 14, 81, 21, 10327, 5, 20000
## $ Perc               <dbl> 21.4285714, 100.0000000, 95.2380952, 0.4260...
## $ PercWeek           <dbl> 21.4285714, 100.0000000, 85.7142857, 0.1936...
## $ PercYear           <dbl> 21.42857, 61.72840, 95.23810, 19.36671, 20....
## $ BehindBy           <dbl> -8.104038, 0.000000, 0.000000, -8.104038, -...
## $ text               <chr> "Need 3 more", "OK!", "OK!", "Need 4141 mor...
## $ BehindFromLastWeek <dbl> 0, 17, 2, 24, 0, 0
```

As we can see...

The variables for the indicators/KPIs are as follows:

* `Actual`: the value of the indicator at the current time of viewing ("Today")
* `Actual_lastWeek`: Last week's value of the indicator
* `Actual_lastYear`: Last year's value of the indicator
* `Target`: the target value for the indicator (used to calculate the percent variables)

Percentages are calculated by:


```r
## Take percents, but protecting against there being no targets
df$Perc <-     df$Actual/(df$Target + 0.0000000000001) * 100
df$PercWeek <- df$Actual_lastWeek/(df$Target + 0.0000000000001) * 100
df$PercYear <- df$Actual_lastYear/(df$Target + 0.0000000000001) * 100

## But truncate results if any are greater than 100
df$Perc[df$Perc > 100] <- 100
df$PercWeek[df$PercWeek > 100] <- 100
df$PercYear[df$PercYear > 100] <- 100
```

* `Perc`: Value of indicator as percent of yearly taget and percent of the year at the current time
* `PercWeek`: Last week's value of the indicator as percent of yearly target and percent of the year
* `PercYear`: Last year's value of the indicator as percent of yearly target and percent of the year

`BehindBy` is calculated by: `df$Perc - PercentTime` and shows how far behind the current value of the indicator is to the target value for the current time

The text can be shown by calculating:


```r
## Calculate how far behind TODAY (VALUE) for text, but remove NAs
df <- df %>%
  mutate(text = PercentTime/100 * df$Target - df$Actual)
df$text[df$BehindBy > 0] <-"OK!"
df$text[df$BehindBy <= 0 & !is.na(df$BehindBy)] <- 
  paste("Need ", round(as.numeric(df$text[df$BehindBy <= 0  & !is.na(df$BehindBy)])), " more", sep = "")
```

* `text` = "OK!": Shows that the current value of the indicator meets the target value for the current time
* `text` = "Need __ more": Shows exactly how much more of the indicator is needed to reach the target value for the current time


Here's one of the versions of the graph!

![](README_files/figure-html/plot-1.png)<!-- -->

For something more similar to Stephen Few's chart there is also this version:

![](README_files/figure-html/multiple_bars-1.png)<!-- -->







