---
title: "PA1_template_EED"
author: "Emily Daniels"
date: "5/8/2020"
output: html_document
---
# REPRODUCIBLE RESEARCH PROJECT 1 {#anchor}  
## Introduction {#css_id}
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. The data is stored in a CSV file and there are 17,568 observations in the dataset. 

The variables in the data are:
* steps
  + Number of steps taken in a 5 minute interval. NAs are coded as "NA"  
* interval
  + Identifyer for the 5 minute interval   
* date
  + The date the measurement was taken in %Y-%m-%d format  
## Loading and Preprocessing the Raw Data {#css_id}
Before loading the data, make sure to install packages that may be helpful for analysis. I loaded the following (but did not use them all): base, DT, dplyr, ggplot2, grDevices, httpuv, lattice, lubridate, nlme, readr, rmarkdown, stats, stringr, tibble, tidyr, tidyverse, utils, wesanderson.

### Install Packages
```{r req packages, include = FALSE}
chooseCRANmirror(graphics=FALSE, ind=1)
  
  install.packages("knitr")
    library(knitr)
    install.packages("rmarkdown")
    library(rmarkdown)
    install.packages("ggplot2")
    library(ggplot2)
    install.packages("httpuv")
    library(httpuv)
    install.packages("lattice")
    library(lattice)
    install.packages("lubridate")
    library(lubridate)
    install.packages("tidyverse")
    library(tidyverse)
    install.packages("dplyr")
    library(dplyr)
    install.packages("wesanderson")
    library(wesanderson)
    install.packages("devtools")
    library(devtools)
    install.packages("DT")
    library(DT)
knitr::opts_chunk$set(echo = TRUE, 
                      eval = TRUE, 
                      message = FALSE,
                      warning = FALSE)

```
#### Data with NAs
```{r walking data NA}
    walkingNA <- read.csv2(unzip("C:\\Users\\emily.e.daniels\\Documents\\R\\Assignments\\Reproducible Research Project 1\\repdata_data_activity.zip", "activity.csv"),sep = ",", nrows = 17568, header = TRUE)
        walkingNA$date <- as.Date(walkingNA$date)
```
#### Data without NAs
```{r walking data NONA}
    walkingNONA <- na.omit(walkingNA)
        walkingNONA$date <- as.Date(walkingNONA$date)
    
```
### What is the mean total number of steps taken per day? {css.class}
* Calculate the total number of steps taken per day without NAs.  
```{r total steps NONA}
    steptotal <- aggregate(walkingNONA$steps, by = list((substr(walkingNONA$date,1,15264))), sum)
        colnames(steptotal) <- c("Date", "Total Steps")

```
* Calculate and report the mean and median of the total number of steps taken each day.  
```{r mean and median NONA}  
## Mean steps
        mean(steptotal$`Total Steps`)
        # 10766.19
## Median steps
        median(steptotal$`Total Steps`)
        #10765     
    
steptotalNA <- aggregate(walkingNA$steps, by = list((substr(walkingNA$interval, 1, 17568))), mean) #na.rm =TRUE
          colnames(steptotalNA) <- c("interval", "steps")
    ```
      
The mean number of steps taken each day: **10766.19**  

The median number of steps taken each day: **10765**    

* Make a histogram of the total number of steps taken each day.  

```{r total steps hist NONA}
data_histogram <- walkingNONA %>%
  mutate(date = factor(date)) %>%
  group_by(date) %>%
  summarize(steps = round(sum(steps),2))
plot.new()

hist(data_histogram$steps, breaks = seq(0, 25000, by = 2000), col = "olivedrab1", xlab = "Total Number of Steps", main = "Total Number of Steps Taken Each Day", xaxt ='n')  
  axis(side=1, at=seq(0,25000, 2000), labels=seq(0,25000,2000))
```
### What is the Average Daily Activity Pattern? {css.class}  

* Make a time series plot ( type = "l") of the 5 minute interval (x) and the average number of steps taken, averaged across all days (y).   

```{r time series}

# Total steps averaged by 5 minute time interval
data_line <- walkingNONA %>%
  mutate(date = factor(date)) %>%
  group_by(interval) %>%
  summarize(steps = round(mean(steps),2))

dat <- ggplot(data_line, aes(interval, steps, color = steps)) + geom_line()
dat

# Total steps averaged by time interval and date
data_line1 <- walkingNONA %>%
  mutate(date = factor(date)) %>%
  group_by(interval, date) %>%
  summarize(steps = round(mean(steps),2))

line <- ggplot(data_line1, aes(interval, steps, color = steps)) + geom_line() + scale_color_gradientn(colors=rainbow(10))
line 

# Total steps by time interval with dated legend 
data_line2 <- walkingNONA %>%
  mutate(date = factor(date)) %>%
  group_by(interval, date) %>%
  summarize(steps = round(mean(steps),2))

line2 <- ggplot(data_line2, aes(interval, steps, color = date)) + geom_line() 
#+ scale_color_gradientn(colors=rainbow(10))
line2 + theme(legend.position = "right", legend.text = element_text(size = 8), legend.box="horizontal" )
```
I decided to explore the time series data and play around with displaying the data in different ways.  

* **Which 5 minute interval, on average across all the says in the dataset, contains the maximum number of steps?**  

```{r max steps}
maxsteps <-  data_line[which.max(data_line$steps), ]$interval
      head(maxsteps) #835
```
    + The 835th interval contains the maximum number of steps.   
    

### Imputing missing values {css.class}
*Imputation* is the process of replacing missing data with substituted values.   

The presence of missing data may introduce bias into some calculations or data summaries. Up until now, we've been working with data containing NO NA values.    

* Calculate and report the total number of missing values in the dataset (**How many rows of NA values are there?**)  

```{r missing values}
    NAsteps <- sum(is.na(walkingNA$steps))
    NAsteps #2304 NA's in steps
```
To fill in the missing values, we'll use the *aggregate()* function to take the mean of steps with NA values when theyre grouped by the 5 minute time interval.  

* Devise a strategy for filling in all the missing values in the dataset.  

```{r aggregate missing}
    stNA <- aggregate(walkingNA$steps, by = list((substr(walkingNA$interval,1,15264))), mean)
        colnames(stNA) <- c("interval", "steps")
        head(stNA)
        
meansteps <- mean(stNA$steps)
        class(meansteps)
        head(meansteps)
```

* Create a new dataset that is equal to the original dataset with the missing data filled in  

```{r new data set}
    newwalk <- walkingNA
# For the cases of newwalk with NAs in the steps column, apply meansteps to fill the NAs
    newwalk[NAsteps, 1] <- meansteps
        head(newwalk)
```

* Make a histogram of the total number of steps taken each day  

```{r hist NAs}
    stepsdaily <- aggregate(steps~date, newwalk, sum)
        head(stepsdaily$steps)

  hist(stepsdaily$steps, main = "Number of Steps Taken Daily- with imputed values", 
     xlab = "Steps", col = wes_palette("GrandBudapest2", 5, type = "continuous"))
```

* Calculate and report the mean and median total number of steps taken per day  

```{r mean and median steps NA}
# Mean steps
    mean(stepsdaily$steps)
    #10766.19
      
# Median steps
     median(stepsdaily$steps)
     #10765
```
      + The mean number of steps taken each day with mean filled NAs: **10766.19**    
      
      + The median number of steps taken each day with mean filled NAs: **10765**   
      
* **Do these values differ from the estimates in the first part of the assignment?**  

+ These values don't differ from the estimates in the first part of the assignment.   

* **What is the impact of imputing missing data on the estimates of the total daily number of steps?**  

+ The imputed data didn't effect the daily average steps enough to significantly change the data.    


### Are there differences in activity patterns between the weekdays and weekends? {css.class}  

* Create a new variable with weekday and weekend levels  

```{r weekdays}
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
weekday <- weekdays(walkingNONA$date, abbreviate = TRUE)
    head(weekday)
 
# Separate out weekends 
walkingNONA$date <- as.Date(walkingNONA$date)   
walkingNONA$day <- weekdays(walkingNONA$date)
for (i in 1:nrow(walkingNONA)) {
    if (walkingNONA[i,]$day %in% c("Saturday","Sunday")) {
        walkingNONA[i,]$day<-"weekend"
    }
    else{
        walkingNONA[i,]$day<-"weekday"
    }
}

dailystep <- aggregate(walkingNONA$steps ~ walkingNONA$interval + walkingNONA$day, walkingNONA, mean)
      names(dailystep) <- c("interval", "day", "steps")
      head(dailystep)
``` 

* Make a line (type = "l") panel plot of the 5 minute interval (x) and the average number of steps taken, averaged across all days (y), separated by weekday and weekend. 

```{r weekday plot}
    wkday <- ggplot(dailystep, aes(x = interval, y = steps, color = day, group = 1)) + geom_line()
      wkday + facet_grid(vars(day))
```

