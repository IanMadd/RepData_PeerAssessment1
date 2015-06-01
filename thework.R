
library(data.table) #need this
library(knitr) # need this
library(lubridate) # need this
library(ggplot2) # need this
library(plyr)

# read the dataset, 3 variables, steps, date, and interval
dataset <- data.table(read.csv("activity.csv"))

#create a data.table with another variable that has Date and Time combined as POSIXct.
dataset <- cbind(dataset, DateTime = as.POSIXct(paste(dataset$date, 
        sprintf("%04d", dataset$interval)), "%Y-%m-%d %H%M", tz = "UTC"))
dataset$Time <- strftime(dataset$DateTime, format="%H:%M:%S", tz="UTC")


##### What is mean total number of steps taken per day?

#Calculates total steps for each day.
StepsPerDaySum <- tapply(dataset$steps, dataset$date, sum)
StepsPerDaySum <- data.table(Date = as.Date(names(StepsPerDaySum)), 
Steps = StepsPerDaySum)



#####Mean steps per day

mean(as.numeric(StepsPerDaySum$Steps), na.rm=TRUE)

#####Median steps per day

median(as.numeric(StepsPerDaySum$Steps), na.rm=TRUE)



###
#Plotting
###

#####Histogram of the frequency of total daily steps:

hist(StepsPerDaySum$Steps, breaks=20, xlab="Number of Steps Taken", 
main="Histogram Showing Frequency of Total Steps Taken", col="lightblue")


p <- ggplot(StepsPerDaySum, aes(x = Steps))
p + geom_histogram(binwidth=1100, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "magenta", high = "blue")



#### What is the average daily activity pattern?
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)

#2. Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?


# New data.table
StepsPerIntervalMean <- tapply(dataset$steps, dataset$Time, mean, na.rm=TRUE)
StepsPerIntervalMean <- data.table(Time = names(StepsPerIntervalMean), Steps = StepsPerIntervalMean)
StepsPerIntervalMean$HourDecimalMin <- as.numeric(levels(as.factor(hour(dataset$DateTime) + minute(dataset$DateTime)/60)))
StepsPerIntervalMean$Interval <- dataset$interval[1:288]

### This is ok but I'd like to change the x-scale

plot(StepsPerIntervalMean$HourDecimalMin, StepsPerIntervalMean$Steps, type="l", col="blue", 
     xlab="Hours", 
     main = "Mean Steps Recorded At 5 Minute Intervals From Midnight to Midnight", 
     ylab="Mean Steps")#THis works

### This plot works!!!.
p <- ggplot(StepsPerIntervalMean, aes(x=HourDecimalMin, y=Steps, group=1))
p + geom_line() + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps in 5 minute intervals from midnight to midnight.")

#### THIS WORKS!
p <- qplot(HourDecimalMin, Steps, data = StepsPerIntervalMean, geom="line", group=1, na.rm=TRUE) 
p + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps in 5 minute intervals from midnight to midnight.")




## the time interval with the max number of steps
StepsPerIntervalMean[StepsPerIntervalMean$Steps==max(StepsPerIntervalMean$Steps)] 



####Imputing missing values

#Note that there are a number of days/intervals where there are missing values 
#(coded as NA). The presence of missing days may introduce bias into some
#calculations or summaries of the data.

#1. Calculate and report the total number of missing values in the dataset
  #(i.e. the total number of rows with NAs)

table(is.na(dataset$steps))


#2. Devise a strategy for filling in all of the missing values in the dataset. 
  #The strategy does not need to be sophisticated. For example, you could use 
  #the mean/median for that day, or the mean for that 5-minute interval, etc.

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



# This creates a new variable "newsteps" which is equal to the original number of steps if that number is available
# or if the steps == NA the newsteps value is the mean of steps for that interval 
for (i in 1:length(dataset$steps)){
  if (is.na(dataset$steps[i])) {
    dataset$newsteps[i] <- StepsPerIntervalMean$Steps[StepsPerIntervalMean$Interval == dataset$interval[i]] 
  }else{
    dataset$newsteps[i] <- dataset$steps[i]
  }
}






#4. Make a histogram of the total number of steps taken each day and Calculate and 
  #report the mean and median total number of steps taken per day. Do these values 
  #differ from the estimates from the first part of the assignment? What is the impact of 
  #imputing missing data on the estimates of the total daily number of steps?




#Are there differences in activity patterns between weekdays and week- ends?
#For this part the weekdays() function may be of some help here. Use the 
#dataset with the filled-in missing values for this part.

#1. Create a new factor variable in the dataset with two levels – “weekday” and 
  #“weekend” indicating whether a given date is a weekday or weekend day.

#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
  #interval (x-axis) and the average number of steps taken, averaged across all 
  #weekday days or weekend days (y-axis). The plot should look something like the 
  #following, which was creating using simulated data:

#Your plot will look different from the one above because you will be using the 
#activity monitor data. Note that the above plot was made using the lattice system 
#but you can make the same version of the plot using any plotting system you choose.













