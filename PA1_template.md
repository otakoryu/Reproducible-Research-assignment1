Reproducible Research 1st assignment
====================================

Loading and preprocessing the data
----------------------------------

``` r
activity<-read.csv("activity.csv",header=TRUE)
activity.complete<-activity[complete.cases(activity),]
```

What is mean total number of steps taken per day?
-------------------------------------------------

1.Calculate the total number of steps taken per day

``` r
total_steps<-aggregate(steps~date,sum,data=activity.complete)
total_steps[1:10,]
```

    ##          date steps
    ## 1  2012-10-02   126
    ## 2  2012-10-03 11352
    ## 3  2012-10-04 12116
    ## 4  2012-10-05 13294
    ## 5  2012-10-06 15420
    ## 6  2012-10-07 11015
    ## 7  2012-10-09 12811
    ## 8  2012-10-10  9900
    ## 9  2012-10-11 10304
    ## 10 2012-10-12 17382

2.Plotting Histogram of the total number of steps taken each day

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.2

``` r
ggplot(total_steps,aes(x=steps))+
    geom_histogram(fill="red",alpha=0.5,position = "dodge")+
    xlab("Steps in a day")+
    ylab("Frequency")+
    labs(title=expression("Total steps taken each day"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

3.Reporting the mean and median of the total number of steps taken per day

``` r
mean(total_steps$steps);median(total_steps$steps)
```

    ## [1] 10766.19

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

1.Make a time series plot of the 5-minute interval and the average number of steps taken,averaged across all days

``` r
avg.total.int<-aggregate(steps~interval,mean,data=activity.complete)

ggplot(avg.total.int,aes(x=interval,y=steps))+
    geom_line(stat = "identity",lwd=1.5,col="green",alpha=0.5)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

2.Which 5-minute inteval, on average across all the days in the dataset,contains the maximum number of steps?

``` r
high<-which(activity.complete==max(activity.complete$steps))
activity.complete[high,]
```

    ##       steps       date interval
    ## 16492   806 2012-11-27      615

The interval of 615 has the highest steps of 806 on average

Imputing mising values
----------------------

1.Calculate and report the total number of missing values in the dataset

``` r
missingvalues<-activity[!complete.cases(activity),]
nrow(missingvalues)
```

    ## [1] 2304

2.Imputing the missing values with mean

``` r
for(i in 1:ncol(activity)){
        activity[is.na(activity[,i]),i]<-mean(activity[,i],na.rm = TRUE)
}
```

    ## Warning in mean.default(activity[, i], na.rm = TRUE): argument is not
    ## numeric or logical: returning NA

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
write.table(activity,file = "activity.imp.txt",quote = F,col.names = T,sep = "|")
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
activity.imp<-read.table("activity.imp.txt",header = TRUE,sep="|")



total_steps.imp<-aggregate(steps~date,data=activity.imp,sum)

mean(total_steps.imp$steps);median(total_steps.imp$steps)
```

    ## [1] 10766.19

    ## [1] 10766.19

``` r
mean(total_steps$steps);median(total_steps$steps)
```

    ## [1] 10766.19

    ## [1] 10765

``` r
ggplot(total_steps.imp,aes(x=steps))+
    geom_histogram(fill="red",alpha=0.5,position="dodge")+
    xlab("Steps in a day")+
    ylab("Frequency")+
    labs(title=expression("Total steps taken each day"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
activity.imp$date<-as.Date(activity.imp$date)
activity.imp$days<-weekdays(activity.imp$date)

weekdays<-subset(activity.imp,!activity.imp$days %in% c("Satturday","Sunday"))
weekend<-subset(activity.imp,activity.imp$days %in% c("Satturday","Sunday"))
```

2.Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

``` r
weekdays.avg<-aggregate(steps~interval,mean,data=weekdays)
weekend.avg<-aggregate(steps~interval,mean,data=weekend)


weekdays.avg$days<-"Weekday"
weekend.avg$days<-"weekend"

weekday.end<-rbind(weekdays.avg,weekend.avg)

ggplot(weekday.end,aes(x=interval,y=steps,col=days))+
    geom_line(stat="identity",alpha=0.8)+
    facet_wrap(~days,scales = "free",nrow=2,ncol=1)+
    xlab("Interval")+
    ylab("Steps")+
    labs(title=expression("Patterns for steps taken during Weekdays and Weekends"))+
    scale_color_discrete(name="Days",labels=c("Weekday","Weekend"))+
        theme(legend.title = element_text(face = "bold"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)
