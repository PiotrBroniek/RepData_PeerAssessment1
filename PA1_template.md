Reproducible Research Project 1 
=======================

This file contains complete code and implementation and was done specifically to be reviewed for completion of Course 
'Reproducible Research' in Data Science path by JHU on Coursera. 

Assignment use data from personal activity and shows number of steps taken per day in 5 minute intervals on 2 month span between October and November 2012.   
Let's begin:   

**Loading and preprocessing the data**      

Data for assigment can be taken from [site] [1]
The variables included in this dataset are:  

*steps*: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
*date*: The date on which the measurement was taken in YYYY-MM-DD format  
*interval*: Identifier for the 5-minute interval in which measurement was taken    

1. Code for reading in the dataset and/or processing the data 

Here, we're going to load some data and preprocess data column.

```{r message = FALSE}
setwd("C:/Users/Piotr/Documents/R/Coursera/Reproducible Research/Reproducible Research")
library(dplyr)
read.csv("activity.csv")->table
tab<-table %>% mutate(date = as.Date(table$date,"%Y-%m-%d"))
```

**What is mean total number of steps taken per day?**  
2. Histogram of the total number of steps taken each day    

I have added mean and median to histogram - blue median and black mean. 
```{r}
tab %>% group_by(date) %>% summarise(avgsteps = mean(steps, na.rm = TRUE),
                                     sumsteps = sum(steps, na.rm = TRUE)) ->tab2
par(mar = c(5,6,4,4))
hist(tab2$sumsteps, col = "red", main = "Histogram of total steps per day", 
     xlab = "Sum of steps per day", ylab = "Frequency", breaks = seq(from = 0, 
     to = max(tab2$sumsteps), by = max(tab2$sumsteps)/10 ))
abline(v = mean(tab2$sumsteps, na.rm = TRUE), col = "blue", lwd = 4)
abline(v = median(tab2$sumsteps, na.rm = TRUE), col = "black", lwd = 4)
```

3.Mean and median number of steps taken each day 

```{r}
mean(tab2$sumsteps, na.rm = TRUE)
median(tab2$sumsteps, na.rm = TRUE)
```

**What is the average daily activity pattern?**  
4. Time series plot of the average number of steps taken

```{r}
tab %>% group_by(interval) %>% summarise(avgsteps = mean(steps, na.rm = TRUE))->tab3
with(tab3,plot(interval,avgsteps, type = "l", main = "Average steps taken each interval"))
```

5. The 5-minute interval that, on average, contains the maximum number of steps  
```{r}
c(tab3$interval[which.max(tab3$avgsteps)],max(tab3$avgsteps))
```
**Imputing missing values**  
6.Code to describe and show a strategy for imputing missing data  
I have used mean for the same interval as impute for NA. Firstly I have counted numbers NA's and create 'tab4' with added tab with means for each interval. In tab5 I have created antoher column that uses original data or mean if NA is missing.
```{r}
sum(is.na(tab$steps))
cbind(tab,rep(tab3$avgsteps,61))->tab4
colnames(tab4)<-c(colnames(tab),"avgstepby5")
cbind(tab,ifelse(is.na(tab4$steps),tab4$avgstepby5,tab4$steps))->tab5
colnames(tab5)<-c(colnames(tab),"steps_NA.RM")
```
7.Histogram of the total number of steps taken each day after missing values are imputed  
I have added summa function to use in tapply function.
```{r}
summa<-function(t){summa<-sum(t, na.rm = TRUE) 
summa}
tapply(tab5$steps_NA.RM,tab5$date,summa)->t_sum
hist(t_sum, col = "red", main = "Histogram of steps taken each day with NA's imput", 
     xlab = "Sum of steps per day", ylab = "Frequency", breaks = seq(from = 0, 
     to = max(t_sum), by = max(t_sum)/10 ))
mean(tapply(tab5$steps_NA.RM,tab5$date,summa))
median(tapply(tab5$steps_NA.RM,tab5$date,summa))
```
**Are there differences in activity patterns between weekdays and weekends?**   
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
Sys.setlocale("LC_TIME", "English")
tab6<-cbind(tab5,weekdays(tab5$date))
tab6<-tab5 %>% mutate(weekday = weekdays(date), 
                typeofday = ifelse((weekday == "Saturday")|(weekday == "Sunday"),
                                                             "weekend","weekday" ))
tab6weekdays<-tab6 %>% filter(typeofday == "weekday")
tab6weekend<-tab6 %>% filter(typeofday == "weekend")
tab6weekdays %>% group_by(interval) %>% 
  summarise(avgsteps = mean(steps_NA.RM)) ->tab7weekdays
tab6weekend %>% group_by(interval) %>% 
  summarise(avgsteps = mean(steps_NA.RM)) ->tab7weekend
par(mfcol = c(2,1))
with(tab7weekdays,plot(interval,avgsteps, type = "l",main = "Average steps on each interval during day on Weekdays"))
with(tab7weekend,plot(interval,avgsteps, type = "l", main = "Average steps on each interval during day on Weekends"))
```


[1]:https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
