---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

```r
unzip("activity.zip",overwrite = TRUE)
dt<-read.csv("./activity.csv",header = TRUE,sep = ",")
dt$date<-as.POSIXct(dt$date,format="%Y-%m-%d")
```




## What is mean total number of steps taken per day?
### 1.Calculate the total number of steps taken per day

```r
totalsteps<-tapply(dt$steps, dt$date, sum)
```
### 2.Make a histogram of the total number of steps taken each day

```r
hist(totalsteps,xlab = "Daily steps",main = "Total daily steps",breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### 3.Calculate and report the mean and median of the total number of steps taken per day

```r
smean<-mean(totalsteps,na.rm = TRUE)
smedian<-median(totalsteps,na.rm = TRUE)
smean
```

```
## [1] 10766.19
```

```r
smedian
```

```
## [1] 10765
```



## What is the average daily activity pattern?
### 1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
smeaninterval<-aggregate(list(meanstep=dt$steps),by=list(meaninterval=dt$interval),mean,na.rm=TRUE)
plot(smeaninterval$meaninterval,smeaninterval$meanstep,type="l",xlab = "interval",ylab = "steps",main = "average steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps<-smeaninterval[which.max(smeaninterval$meanstep),]
maxsteps
```

```
##     meaninterval meanstep
## 104          835 206.1698
```

## Imputing missing values
### 1.Calculate and report the total number of missing values in the dataset

```r
sumna<-sum(is.na(dt$steps))
sumna
```

```
## [1] 2304
```

### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdt<-dt
na<-which(is.na(dt$steps))
meantona<-rep(mean(newdt$steps,na.rm=TRUE,times=length(na)))
newdt[na,"steps"]<-meantona
```

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newtotalsteps<-tapply(newdt$steps, newdt$date, sum)
hist(newtotalsteps,xlab = "Daily steps (new)",main = "Total daily steps",breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
newmean<-mean(newtotalsteps)
newmedian<-median(newtotalsteps)
newmean
```

```
## [1] 10766.19
```

```r
newmedian
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
newdt$datetype<-ifelse(as.POSIXlt(newdt$date)$wday %in% c(0,6),"weekend","weekday")
```

### 2.Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
new<-aggregate(x=list(nstep=newdt$steps),by=list(ninterval=newdt$interval,ndatetype=newdt$datetype),mean)

library(lattice)

xyplot(new$nstep~new$ninterval|new$ndatetype,type="l",xlab = "interval",ylab = "steps",main = "average steps by datetype",layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

