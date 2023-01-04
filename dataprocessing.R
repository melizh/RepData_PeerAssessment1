rm(list=ls())

# load libraries
library(dplyr)

# Read in dataset
datadest <- "activity.zip"
dataname <- unzip(datadest, list=TRUE)$Name
unzip(datadest)
dforig <- read.csv(dataname)
df <- dforig[!is.na(dforig$steps),]

# mean total steps per day
stepsums <- as.data.frame(df %>% group_by(date) %>% summarise(totalsteps=sum(steps), .groups='drop'))
meansteps <- mean(stepsums$totalsteps)
medsteps <- median(stepsums$totalsteps)

# average daily activity pattern
stepmeans <- as.data.frame(df %>% group_by(interval) %>% summarise(meansteps=mean(steps), .groups='drop'))
maxstepint <- stepmeans[stepmeans$meansteps == max(stepmeans$meansteps),]

# imputing missing values (used rounded mean of the interval)
totalmissingvals <- sum(is.na(dforig$steps))
uints <- unique(dforig$interval)
dffilled <- read.csv(dataname)
for (tpts in uints) {
  dffilled[dffilled$interval == tpts & is.na(dffilled$steps),1] = round(stepmeans[stepmeans$interval == tpts,2])
}
totalmissingvals1 <- sum(is.na(dffilled$steps))

# mean and median with data filled: total steps per day
stepsumsf <- as.data.frame(dffilled %>% group_by(date) %>% summarise(totalsteps=sum(steps), .groups='drop'))
meanstepsf <- mean(stepsumsf$totalsteps)
medstepsf <- median(stepsumsf$totalsteps)

# weekday/weekend differences
dffilled$date <- as.Date(dffilled$date)
dffilled <- dffilled %>% mutate(daytype = factor(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", labels = c("weekday","weekend")))
avgww <- as.data.frame(dffilled %>% group_by(interval,daytype) %>% summarise(meansteps=mean(steps), .groups='drop'))
avgww <- avgww[order(avgww$daytype,avgww$interval),]
