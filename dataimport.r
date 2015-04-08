# extract data from the zipped file and read into an r data frame
unzip("activity.zip")
attach(activity)
activity <- read.csv('activity.csv')