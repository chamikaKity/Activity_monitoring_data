Data
----

Dataset: [Activity monitoring
data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values
    are coded as NA)
-   date: The date on which the measurement was taken in YYYY-MM-DD
    format
-   interval: Identifier for the 5-minute interval in which measurement
    was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

1.  Loading the data

<!-- -->

    data <- read.csv("activity.csv")
    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

1.  Processing the data

<!-- -->

    data$date <- as.Date(data$date,"%Y-%m-%d")

What is mean total number of steps taken per day?
-------------------------------------------------

Ignore the missing values in the dataset.

    dataWithoutNA <- na.omit(data)

1.  Calculate the total number of steps taken per day

<!-- -->

    uniqueDate <- unique(dataWithoutNA$date)
    totalSteps <- vector(mode="integer", length=length(uniqueDate))
    for (i in 1:length(uniqueDate)){
            totalSteps[i]=sum(subset(dataWithoutNA,dataWithoutNA$date==uniqueDate[i])$steps)
    }
    df <- data.frame(uniqueDate,totalSteps)

1.  Make a histogram of the total number of steps taken each day Ignore
    the missing values in the dataset.

<!-- -->

    hist(df$totalSteps,xlab = "Total Steps", main="Histogram of total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/histogram%20total%20steps-1.png)

3.Calculate the mean and median of the total number of steps taken per
day

    mean(df$totalSteps)

    ## [1] 10766.19

    median(df$totalSteps)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

The average daily activity pattern

    uniqueInterval <- unique(dataWithoutNA$interval)
    averageSteps <- vector(mode="integer", length=length(uniqueInterval))
    for (i in 1:length(uniqueInterval)){
            averageSteps[i]=mean(subset(dataWithoutNA,dataWithoutNA$interval==uniqueInterval[i])$steps)
    }
    df2 <- data.frame(uniqueInterval,averageSteps)

1.  Time series plot of the 5-minute interval (x-axis) and the average
    number of steps taken, averaged across all days (y-axis)

<!-- -->

    plot(df2$uniqueInterval,df2$averageSteps,type = "l",xlab = "5-minute Interval",
         ylab = "Average number of steps",main = "Time series plot of the average number of steps taken across all days")  

![](PA1_template_files/figure-markdown_strict/time%20series%20plot-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    df2$uniqueInterval[df2$averageSteps==max(df2$averageSteps)]

    ## [1] 835

Imputing missing values
-----------------------

1.  Calculate the total number of missing values in the dataset.

<!-- -->

    sum(is.na(data))

    ## [1] 2304

1.  Strategy for filling in all of the missing values in the dataset:
    Use the mean for that 5-minute interval

2.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    aproxSteps <- vector(mode="integer", length=length(data$steps))
    for (i in 1:length(aproxSteps)){
            if(is.na(data$steps[i])==TRUE){
                    aproxSteps[i] = df2$averageSteps[data$interval[i]==df2$uniqueInterval]
            }else{
                    aproxSteps[i] = data$steps[i]
            }
    }
    dataFilledNA <- data.frame(aproxSteps,data$date,data$interval)
    colnames(dataFilledNA) <- c("steps","date","interval")

1.  Make a histogram of the total number of steps taken each day.

<!-- -->

    newUniqueDate <- unique(dataFilledNA$date)
    newTotalSteps <- vector(mode="integer", length=length(newUniqueDate))
    for (i in 1:length(newUniqueDate)){
            newTotalSteps[i]=sum(subset(dataFilledNA,dataFilledNA$date==newUniqueDate[i])$steps)
    }
    newdf <- data.frame(newUniqueDate,newTotalSteps)

Histogram

    hist(newdf$newTotalSteps,xlab = "Total Steps", main="Histogram of total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/hist%20all%20data%20-1.png)

The mean and median of the total number of steps taken per day

    mean(newdf$newTotalSteps)

    ## [1] 10766.19

    median(newdf$newTotalSteps)

    ## [1] 10766.19

Do these values differ from the estimates from the first part of the
assignment? What is the impact of imputing missing data on the estimates
of the total daily number of steps? Mean did not change. Median changed
from 10765 to 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    dataFilledNA$day <- factor((weekdays(dataFilledNA$date) %in% weekdays),levels=c(FALSE, TRUE), labels=c("weekend", "weekday")) 

1.  Make a panel plot containing a time series plot of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all weekday days or weekend days (y-axis).

Create a new data frame

    newUniqueInterval <- unique(dataFilledNA$interval)
    len <- length(newUniqueInterval)
    newAverageSteps <- vector(mode="integer", length=len*2)
    newDay <- vector(mode="character", length=len*2)
    newInterval <- vector(mode="integer", length=len*2)
    for (i in 1:len){
            newAverageSteps[i]=mean(subset(dataFilledNA,dataFilledNA$interval==newUniqueInterval[i] & dataFilledNA$day=="weekday")$steps)
            newDay[i]="weekday"
            newInterval[i]=newUniqueInterval[i]
            
            newAverageSteps[i+len]=mean(subset(dataFilledNA,dataFilledNA$interval==newUniqueInterval[i] & dataFilledNA$day=="weekend")$steps)
            newDay[i+len]="weekend"
            newInterval[i+len]=newUniqueInterval[i]
            
    }
    newDay <- as.factor(newDay)
    df3 <- data.frame(newAverageSteps,newInterval,newDay)
    colnames(df3) <- c("steps","interval","day")

Time series plot

    library(lattice)
    plot3 <- xyplot(steps~interval|day,data = df3, type="l",main=" The average number of steps taken per 5-minute \n interval across weekdays and weekends",
           xlab = "Interval", ylab = "Number of steps")

    plot(plot3)

![](PA1_template_files/figure-markdown_strict/panel%20plot-1.png)
