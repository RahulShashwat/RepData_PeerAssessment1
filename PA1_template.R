##Setting Working Directory
setwd("C:/Users/User/Desktop/Coursera Data/Course 5/Week2")

##Loading and preprocessing the data
#Reading file in R
data_activity <- read.csv("activity.csv")

#Reading the data file in R
data_activity <- read.csv("activity.csv")

#What is mean total number of steps taken per day?
###Histogram of the total number of steps taken each day 

#Finding out total number of steps per day
data_steps_Per_Day <- aggregate(steps ~ date, data_activity, sum,na.rm = TRUE)
pdf("plot1.pdf")
hist(data_steps_Per_Day$steps, main = "Steps per day", xlab = "Steps", col = "orange", breaks = 8)
dev.off()
###Calculate and report the mean and median of the total number of steps taken per day

meanStepsPerDay <- mean(data_steps_Per_Day$steps)
medianStepsPerDay <- median(data_steps_Per_Day$steps)


#What is the average daily activity pattern?
###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

data_steps_Interval <- aggregate(steps ~ interval, data_activity, mean,na.rm = TRUE)
pdf("plot2.pdf")
plot(data_steps_Interval$interval, data_steps_Interval$steps, xlab = "Interval", ylab = "Steps", main  = "Steps Per Interval", type = "l", col = "blue")
dev.off()

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_interval_steps <- data_steps_Interval[which.max(data_steps_Interval$steps),]$interval

#Imputing missing values
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

#Finding number of rows with Missing Values
missing_rows <- nrow(data_activity[is.na(data_activity$steps),])

###Devise a strategy for filling in all of the missing values in the dataset. 

#Creating function "meanStepsPerInterval" which takes Date as input and returns mean steps for that Date
#It uses the dataset "stepsPerDay" which was created above
meanStepsPerInterval<-function(interval){
  data_steps_Interval[data_steps_Interval$interval == interval,]$steps
}

###Create a new dataset that is equal to the original dataset but with the missing data filled in
# Make a new dataset with the original data
data_activity_no_NA<-data_activity   
count=0           # Count the number of data filled in
#Replacing the NA values with the mean number of steps for that day
for(i in 1:nrow(data_activity_no_NA)){
  if(is.na(data_activity_no_NA[i,]$steps)){
    data_activity_no_NA[i,]$steps<-meanStepsPerInterval(data_activity_no_NA[i,]$interval)
    count=count+1
  }
}
cat("Total ",count, "NA values were filled.\n\r")  

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

data_Steps_Per_Day_no_NA<-aggregate(steps~date,data=data_activity_no_NA,sum)
pdf("plot3.pdf")
hist(data_Steps_Per_Day_no_NA$steps,main = "Steps per day (no missing values)", xlab = "Steps", col = "orange")
dev.off()
mean_steps_per_day_no_NA <- mean(data_Steps_Per_Day_no_NA$steps)
median_steps_per_day_no_NA <- median(data_Steps_Per_Day_no_NA$steps)


#Are there differences in activity patterns between weekdays and weekends?
###Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or a weekend day

#Adding field day name to the dataset
data_activity_no_NA$dayname <- weekdays(as.Date(data_activity_no_NA$date))
#Adding field day type to the dataset having levels weekday or weekend
data_activity_no_NA$daytype <- as.factor(ifelse(data_activity_no_NA$dayname %in% c('Saturday', 'Sunday'), "Weekend","Weekday"))

###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

data_activity_no_NA_day_type=aggregate(steps~interval+daytype,data_activity_no_NA,mean)
library(lattice)
pdf("plot4.pdf")
xyplot(steps~interval|factor(daytype),data=data_activity_no_NA_day_type,aspect=1/2,type="l",xlab = "Interval", ylab = "Number of Steps")
dev.off()


