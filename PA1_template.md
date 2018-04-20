This markdown file was produced for the first assignment in the
Reproducible Research course.

##### Load the the activity data

-   read in the csv file

<!-- -->

    act<-read.csv("activity.csv",colClasses="character")
    head(act)

    ##   steps       date interval
    ## 1  <NA> 2012-10-01        0
    ## 2  <NA> 2012-10-01        5
    ## 3  <NA> 2012-10-01       10
    ## 4  <NA> 2012-10-01       15
    ## 5  <NA> 2012-10-01       20
    ## 6  <NA> 2012-10-01       25

-   set appropriate classes for ech column of data

<!-- -->

    act[,1] <- as.numeric(act[,1])
    act[,3] <- as.numeric(act[,3])
    str(act)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...

##### Look at daily values:

-   make a clean copy of the data (removing rows with NA data)

<!-- -->

    cleanAct <- na.omit(act)

-   compute the total steps per day

<!-- -->

    totalStepsDay <- tapply(cleanAct$steps,cleanAct$date,sum)
    head(totalStepsDay)

    ## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
    ##        126      11352      12116      13294      15420      11015

-   generate a histogram of total steps per day
    ![](PA1_template_files/figure-markdown_strict/hist_daily_steps-1.png)
-   identify the mean and median total steps per day

<!-- -->

    mean(totalStepsDay)

    ## [1] 10766.19

    median(totalStepsDay)

    ## [1] 10765

##### Look at the daily activity pattern:

-   compute the number of steps per interval averaged across days

<!-- -->

    meanStepsInterval <-tapply(cleanAct$steps,cleanAct$interval,mean)
    meanStepsInterval<-data.frame(interval=as.numeric(rownames(meanStepsInterval)),steps=meanStepsInterval)
    head(meanStepsInterval)

    ##    interval     steps
    ## 0         0 1.7169811
    ## 5         5 0.3396226
    ## 10       10 0.1320755
    ## 15       15 0.1509434
    ## 20       20 0.0754717
    ## 25       25 2.0943396

-   create a time series plot of the steps per interval
    ![](PA1_template_files/figure-markdown_strict/plot_interval_steps-1.png)
-   identify the interval with the highest average step count

<!-- -->

    meanStepsInterval$interval[which.max(meanStepsInterval$steps)]

    ## [1] 835

-   and the corresponding step count

<!-- -->

    max(meanStepsInterval$steps)

    ## [1] 206.1698

##### Fill in mising values:

-   identify the number of rows with NA data

<!-- -->

    nrow(act)-nrow(cleanAct)

    ## [1] 2304

-   create a copy of the dataset

<!-- -->

    fixedAct<-act

-   replace NA values with the mean number of steps for the given
    interval (averaged across all days)

<!-- -->

    for (row in 1:nrow(fixedAct)) {
        if (is.na(fixedAct$steps[row])) {
            fixedAct$steps[row] <- round(meanStepsInterval$steps[(as.character(fixedAct$interval[row]))])
        }
    }

##### Reanalyze the "fixed" daily data

-   compute total steps per day

<!-- -->

    totalStepsDay2 <- tapply(fixedAct$steps,fixedAct$date,sum)
    head(totalStepsDay2)

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##      10762        126      11352      12116      13294      15420

-   generate a histogram of daily steps
    ![](PA1_template_files/figure-markdown_strict/hist_fixed_daily_steps-1.png)
-   identify the mean daily steps

<!-- -->

    mean(totalStepsDay2)

    ## [1] 10765.64

-   identify the median daily steps

<!-- -->

    median(totalStepsDay2)

    ## [1] 10762

-   compare to cleaned data

<!-- -->

    mean(totalStepsDay2) / mean(totalStepsDay)

    ## [1] 0.999949

    median(totalStepsDay2) / median(totalStepsDay)

    ## [1] 0.9997213

##### Reanalyze the "fixed" interval data

-   compute count of fixed steps per interval averaged across all days

<!-- -->

    meanStepsInterval2 <-round(tapply(fixedAct$steps,fixedAct$interval,mean))
    meanStepsInterval2<-data.frame(interval=as.numeric(rownames(meanStepsInterval2)),steps=meanStepsInterval2)
    head(meanStepsInterval2)

    ##    interval steps
    ## 0         0     2
    ## 5         5     0
    ## 10       10     0
    ## 15       15     0
    ## 20       20     0
    ## 25       25     2

-   create a time series plot of the steps per interval
    ![](PA1_template_files/figure-markdown_strict/plot_fixed_interval_steps-1.png)
-   identify the interval with the highest average step count

<!-- -->

    meanStepsInterval2$interval[which.max(meanStepsInterval2$steps)]

    ## [1] 835

    max(meanStepsInterval2$steps)

    ## [1] 206

-   compare to cleaned data

<!-- -->

    meanStepsInterval2$interval[which.max(meanStepsInterval2$steps)] / meanStepsInterval$interval[which.max(meanStepsInterval$steps)]

    ## [1] 1

    max(meanStepsInterval2$steps) / max(meanStepsInterval$steps)

    ## [1] 0.9991764

##### Organize data by weekday Vs weekend

-   add a factor indicating whether a given date is a weekday or a
    weekend

<!-- -->

    type<-as.character()
    days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    for (i in 1:5) type[days[i]] <- "weekday"
    for (i in 6:7) type[days[i]] <- "weekend"
    fixedAct$dayType <- type[weekdays(as.Date(fixedAct$date))]

    ## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
    ## 'zone/tz/2018c.1.0/zoneinfo/America/New_York'

    fixedAct$dayType <- as.factor(fixedAct$dayType)

-   create subsets of the data for weekdays vs weekends

<!-- -->

    fixedActWD<-subset(fixedAct,as.character(fixedAct$dayType)=="weekday")
    meanStepsIntervalWD <-round(tapply(fixedActWD$steps,fixedActWD$interval,mean))
    meanStepsIntervalWD<-data.frame(interval=as.numeric(as.character(rownames(meanStepsIntervalWD))),steps=meanStepsIntervalWD)
    fixedActWE<-subset(fixedAct,as.character(fixedAct$dayType)=="weekend")
    meanStepsIntervalWE <-round(tapply(fixedActWE$steps,fixedActWE$interval,mean))
    meanStepsIntervalWE<-data.frame(interval=as.numeric(as.character(rownames(meanStepsIntervalWE))),steps=meanStepsIntervalWE)

##### Compare weekday to weekend steps

-Generate a time series panel plot per the read.me example that compares
the mean steps/interval for weekdays and weekends.
![](PA1_template_files/figure-markdown_strict/panelplot-1.png)
