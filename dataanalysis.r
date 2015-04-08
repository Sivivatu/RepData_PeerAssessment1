#data download, extraction and library package loading

if (!exists('activity')) {
  destfile <- "activity.csv"
  url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url, "repdata-data-activity.zip", mode="wb")
  unzip("repdata-data-activity.zip")
  activity <- read.csv(destfile)
  remove(destfile)
}

p <- c('plyr', 'ggplot2', 'Rcpp')
lapply(p, library, character.only=T)

#summary analysis of the data to calculate the total, mean, and median daily steps
dailysteps <- ddply(activity, .(date) , summarize, daysteps =sum(steps))
total_steps <- sum(activity$steps, na.rm=TRUE)
mean_daily_steps <- round(mean(dailysteps$daysteps, na.rm = TRUE),2)
median_daily_steps <- median(dailysteps$daysteps, na.rm = TRUE)
qplot(daysteps, data=dailysteps, geom='histogram')

#print out the summary data for the analysis
summary <- data.frame(total_steps, mean_daily_steps, median_daily_steps)
print(summary)

#timeseries plot of 5min interval steps taken
intradaysteps <- ddply(activity, .(interval) , summarize, steps =mean(steps, na.rm = TRUE))
qplot(interval, steps, data=intradaysteps, geom="line")

#return the 5min interval with the maximum average steps each day
sortedinterval <- intradaysteps[rev(order(intradaysteps$steps)),]
sortedinterval[1,]

#count of NAs in dataset
countNA <- sum(is.na(activity$steps))

#replace the NA fields with the mean for that time interval using the optmatch package
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activityNA <- ddply(activity, ~ interval, transform, stepsna = impute.mean(steps))

activityNA[order(activityNA$date), ] #plyr orders by group so we have to reorder

#copy of steps from question 1 replacing the data set for the updated NA cleaned data
dailystepsNA <- ddply(activityNA, .(date) , summarize, daysteps =sum(stepsna))
mean_daily_stepsNA <- round(mean(dailystepsNA$daysteps, na.rm = TRUE),2)
median_daily_stepsNA <- median(dailystepsNA$daysteps, na.rm = TRUE)
qplot(daysteps, data=dailystepsNA, geom='histogram')

#print out the summary data for the analysis
summaryNA <- data.frame(CountNA, mean_daily_stepsNA, median_daily_stepsNA)
print(summaryNA)

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
