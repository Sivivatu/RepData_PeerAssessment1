Loading and preprocessing the data
----------------------------------

The activity data is downloaded from the assessment link provided in the REAdME.md file.
In addition the packages that are used are loaded into the environment

``` {.r}
#checks if activity object exists, if not downloads the data and extracts from the zip file 
if (!exists('activity')) {
  destfile <- "activity.csv"
  url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, "repdata-data-activity.zip", mode="wb")
  unzip("repdata-data-activity.zip")
  activity <- read.csv(destfile)
  remove(destfile)
}

#library package loading
p <- c('plyr', 'ggplot2', 'Rcpp')
lapply(p, library, character.only=T)
```

What is mean total number of steps taken per day?
-------------------------------------------------

This assessment requires the calculation of the total number of steps per day. i.e. sum the data across the day and take the mean across all days
Additionally a histogram for the intraday steps (across the intervals each day) is produced.

``` {.r}
#summary analysis of the data to calculate the total, mean, and median daily steps
dailysteps <- ddply(activity, .(date) , summarize, daysteps =sum(steps))
total_steps <- sum(activity$steps, na.rm=TRUE)
mean_daily_steps <- round(mean(dailysteps$daysteps, na.rm = TRUE),2)
median_daily_steps <- median(dailysteps$daysteps, na.rm = TRUE)

#create a histogram of the mean daily steps taken
qplot(daysteps, data=dailysteps, geom='histogram')
```

![](PA1_template_files/figure-markdown_github/daily%20steps%20analysis-1.png)

``` {.r}
png("figure/dailyanalysis.png")
qplot(daysteps, data=dailysteps, geom='histogram')
dev.off()
```

    ## png 
    ##   2

``` {.r}
#print out the summary data for the analysis
summary <- data.frame(total_steps, mean_daily_steps, median_daily_steps)
print(summary)
```

    ##   total_steps mean_daily_steps median_daily_steps
    ## 1      570608         10766.19              10765

What is the average daily activity pattern?
-------------------------------------------

Creates a single line graph displaying the steps take across the day's interval pattern From this and assessment of the maximum steps taking in a individual interval is take and printed to the display

``` {.r}
#timeseries plot of 5min interval steps taken
intradaysteps <- ddply(activity, .(interval) , summarize, steps =mean(steps, na.rm = TRUE))
qplot(interval, steps, data=intradaysteps, geom="line")
```

![](PA1_template_files/figure-markdown_github/daily%20pattern%20(intraday)-1.png)

``` {.r}
png("figure/intraday.png")
qplot(interval, steps, data=intradaysteps, geom="line")
dev.off()
```

    ## png 
    ##   2

``` {.r}
#return the 5min interval with the maximum average steps each day
sortedinterval <- intradaysteps[rev(order(intradaysteps$steps)),]
sortedinterval[1,]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

The missing values are replaced with the mean for the time interval across the data set. This result is then used for the subsequent analysis. A repeat of the daily analysis in Question 1 is repeated on the NA cleaned data.

``` {.r}
countNA <- sum(is.na(activity$steps))

#replace the NA fields with the mean for that time interval using the optmatch package
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activityNA <- ddply(activity, ~ interval, transform, stepsna = impute.mean(steps))

#copy of steps from question 1 replacing the data set for the updated NA cleaned data
dailystepsNA <- ddply(activityNA, .(date) , summarize, daysteps =sum(stepsna))
mean_daily_stepsNA <- round(mean(dailystepsNA$daysteps, na.rm = TRUE),2)
median_daily_stepsNA <- median(dailystepsNA$daysteps, na.rm = TRUE)
qplot(daysteps, data=dailystepsNA, geom='histogram')
```

![](PA1_template_files/figure-markdown_github/NA%20removal-1.png)

``` {.r}
png("figure/dailyanalysisNA.png")
qplot(daysteps, data=dailystepsNA, geom='histogram')
dev.off()
```

    ## png 
    ##   2

``` {.r}
#print out the summary data for the analysis
summaryNA <- data.frame(countNA, mean_daily_stepsNA, median_daily_stepsNA)
print(summaryNA)
```

    ##   countNA mean_daily_stepsNA median_daily_stepsNA
    ## 1    2304           10766.19             10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

A new 2 factor flag ('weekendflag') is created to separate weekdays from weekends. This field is used to create a comparasion between the steps pattern for weekdays versus weekends

``` {.r}
#calculated the difference between weekday and weekend activity
#3 part clean up process 
#   1. Convert the date field to a date class
#   2. Create the  factor flag for weekend/weekday
#   3. Convert the weekend flag to a factor class
activityNA$date <- as.Date(activityNA$date)
activityNA$weekendflag <- ifelse(weekdays(activityNA$date) %in% c('Saturday', 'Sunday'),'weekend', 'weekday') 
activityNA$weekendflag <- as.factor(activityNA$weekendflag)

# Create the 2 level lattice plot using ggplot2
qplot(interval, steps, facets = weekendflag~., data=activityNA, geom= 'line')
```

![](PA1_template_files/figure-markdown_github/weekday%20variance-1.png)

``` {.r}
png("figure/weekdayNA.png")
qplot(interval, steps, facets = weekendflag~., data=activityNA, geom= 'line')
dev.off()
```

    ## png 
    ##   2

The comparasion line graphs show weekdays have higher step counts across the day, especially earlier in the day. While weekends have a similar period maximum, it happens later in the day and that maximum is not sustained for as long.
