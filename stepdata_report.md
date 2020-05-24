    knitr::opts_chunk$set(echo = TRUE)
    install.packages('dplyr', repos = "http://cran.us.r-project.org")

    ## Installing package into 'C:/Users/MAHE/Documents/R/win-library/3.6'
    ## (as 'lib' is unspecified)

    ## 
    ##   There is a binary version available but the source version is later:
    ##       binary source needs_compilation
    ## dplyr  0.8.4  0.8.5              TRUE
    ## 
    ##   Binaries will be installed
    ## package 'dplyr' successfully unpacked and MD5 sums checked

    ## Warning: cannot remove prior installation of package 'dplyr'

    ## Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying C:
    ## \Users\MAHE\Documents\R\win-library\3.6\00LOCK\dplyr\libs\x64\dplyr.dll to C:
    ## \Users\MAHE\Documents\R\win-library\3.6\dplyr\libs\x64\dplyr.dll: Permission
    ## denied

    ## Warning: restored 'dplyr'

    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\MAHE\AppData\Local\Temp\RtmpqAYAGW\downloaded_packages

    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)

1.  Code for reading in the dataset and/or processing the data

<!-- -->

    act<-read.csv("activity.csv")
    act.complete <- na.omit(act)   #Creates data frame with no NA values 

1.  Histogram of the total number of steps taken each day

<!-- -->

    dates<-factor(act$date)
    total<-tapply(act$steps,dates,sum)
    hist(total, xlab = "Total daily Steps",main="Histogram of Total Steps by day",breaks=20)

![](stepdata_report_files/figure-markdown_strict/unnamed-chunk-2-1.png)

1.  Mean and median number of steps taken each day

<!-- -->

    total<-as.data.frame.table(total)               #converts from array to data frame
    colnames(total)<-c("date","total steps")

    total.steps<-total[complete.cases(total),]           #Very Important
    mean(total.steps$`total steps`)

    ## [1] 10766.19

    median(total.steps$`total steps`)

    ## [1] 10765

1.  Time series plot of the average number of steps taken

<!-- -->

    act.interval<-group_by(act.complete,interval)  #Very useful function in dplyr library
    act.interval<-summarize(act.interval,steps=mean(steps))
    ggplot(act.interval, aes(interval, steps)) + geom_line()

![](stepdata_report_files/figure-markdown_strict/unnamed-chunk-4-1.png)

1.  The 5-minute interval that, on average, contains the maximum number
    of steps

<!-- -->

    act.interval[act.interval$steps==max(act.interval$steps),]

    ## # A tibble: 1 x 2
    ##   interval steps
    ##      <int> <dbl>
    ## 1      835  206.

1.  Code to describe and show a strategy for imputing missing data

<!-- -->

    nrow(act)-nrow(act.complete)   #no. of rows with NA vals

    ## [1] 2304

    names(act.interval)[2] <- "mean.steps"   #Important
    act.impute <- merge(act, act.interval)  #merging allows the 2 data frames to be merged together                                           on the basis of common rows and/or columns
    act.impute$steps[is.na(act.impute$steps)] <- act.impute$mean.steps[is.na(act.impute$steps)]  
    #upper line produces dataframe with NA values replaced with mean val of steps for that interval

1.  Histogram of the total number of steps taken each day after missing
    values are imputed

<!-- -->

    act.day.imp <- group_by(act.impute, date)
    act.day.imp <- summarize(act.day.imp, steps=sum(steps))

    qplot(steps, data=act.day.imp)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](stepdata_report_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends

<!-- -->

    act.impute$dayofweek <- weekdays(as.Date(act.impute$date))
    act.impute$weekend <-as.factor(act.impute$dayofweek=="Saturday"|act.impute$dayofweek=="Sunday")
    levels(act.impute$weekend) <- c("Weekday", "Weekend")
    act.weekday <- act.impute[act.impute$weekend=="Weekday",]
    act.weekend <- act.impute[act.impute$weekend=="Weekend",]

    act.int.weekday <- group_by(act.weekday, interval)
    act.int.weekday <- summarize(act.int.weekday, steps=mean(steps))
    act.int.weekday$weekend <- "Weekday"
    act.int.weekend <- group_by(act.weekend, interval)
    act.int.weekend <- summarize(act.int.weekend, steps=mean(steps))
    act.int.weekend$weekend <- "Weekend"

    act.int <- rbind(act.int.weekday, act.int.weekend)
    act.int$weekend <- as.factor(act.int$weekend)
    ggplot(act.int, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)

![](stepdata_report_files/figure-markdown_strict/unnamed-chunk-8-1.png)
