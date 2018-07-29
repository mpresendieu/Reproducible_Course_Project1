#Course Project #1

#Load package
library(readr);library(dplyr)
library(readxl);library(ggplot2)
library(stringr);library(plyr)

#load activity_monitoring_data
setwd("~/Coursera/Reproducibile Research/Week_2/RepData_PeerAssessment1")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "step_data.zip"
download.file(url, destfile)
unzip(destfile)
act_data <- read.csv("activity.csv", sep = ",", stringsAsFactors = FALSE)

#processing the data w/ variables (day & date_time)
act_data$day <- weekdays(as.Date(act_data$date))
act_data$date_time <- as.POSIXct(act_data$date, format= "%Y-%m-%d")

##removing NAS from the data
act_data1 <- act_data[!is.na(act_data$steps),]


#mean total number of steps per day
##summarizing total steps per date
act_table_sum <- aggregate(act_data1$steps ~ act_data1$date, FUN = sum)
colnames(act_table_sum) <- c("Date", "Steps")
#make a histogram of the total # of steps taken each day
hist(act_table_sum$Steps, main ="Total # of Steps Per Day", xlab = "Steps", col = "red", breaks = 5)
#calculate the mean of the total # of steps taken per day
as.integer(mean(act_table_sum$Steps))
#calculate the median of the total # of steps taken per day
as.integer(median(act_table_sum$Steps))


#what is the average daily activity pattern
##create avg. # of steps per interval
library(plyr)
library(ggplot2)
##removing NAS from the data
act_data1 <- act_data[!is.na(act_data$steps),]
inv_table <- ddply(act_data1, .(interval), summarize, Avg = mean(steps))
##create line plot og avg. ## of step per interval
time_plot <- ggplot(inv_table, aes(x = interval, y= Avg), xlab = "Interval", ylab= "Avg. # of Steps")
time_plot + geom_line() + xlab("Interval") + ylab ("Avg. # of Steps") + ggtitle("Average Number of Steps per Interval")
##maximum steps by interval
max_steps <- max(inv_table$Avg)
##which interval contains the maximum avg. # of steps
inv_table[inv_table$Avg == max(max_steps),1]


#Imputing missing values
#calculate and report the total number of missing values
sum(is.na(act_data))

##create the avg. # of steps per weekday and interval
df_act <- act_data
miss_df <- is.na(df_act$steps)
int_avg <- tapply(df_act$steps, df_act$interval, mean, na.rm = TRUE, simplify = T)
df_act$steps[miss_df] <- int_avg[as.character(df_act$interval[miss_df])]

#make a histogram of the total # of steps taken each
new_daily <- tapply(df_act$steps, df_act$date, sum, na.rm = TRUE, simplify = T)
hist(x = new_daily, col = "blue", breaks =20, xlab = "daily steps", ylab ="frequency", main = "The distribution of daily total w/ missing data imputed")

#mean of new_daily
round(mean(new_daily), digits = 0)

#median of new_daily
round(median(new_daily), digits = 0)

##Are these differences in activity patterns b/t weekdays and weekends?
#create a helper functions to decide if a day is a week day or not
is_weekday <- function(d) {
        wd <- weekdays(d, abbreviate= FALSE)
        ifelse(wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}
wm <- sapply(df_act$date_time, is_weekday)
df_act$wk <- as.factor(wm)

##make a panel plot containing a time series plot
wk_df <- aggregate(steps ~ wk + interval, data = df_act, FUN = mean)

library(lattice)
xyplot(steps ~ interval | factor(wk), layout = c(1,2), xlab="Interval", ylab= "# of Steps", type ="l", data= wk_df)
