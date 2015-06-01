
library(data.table) #need this
library(knitr) # need this
library(lubridate) # need this
library(ggplot2) # need this
library(dplyr) # need this

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
p + geom_histogram(binwidth=1100)



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
    dataset$calculatedSteps[i] <- StepsPerIntervalMean$Steps[StepsPerIntervalMean$Interval == dataset$interval[i]] 
  }else{
    dataset$calculatedSteps[i] <- dataset$steps[i]
  }
}




#4. Make a histogram of the total number of steps taken each day and Calculate and 
  #report the mean and median total number of steps taken per day. Do these values 
  #differ from the estimates from the first part of the assignment? What is the impact of 
  #imputing missing data on the estimates of the total daily number of steps?

StepsPerDaySum$calculatedSteps <- tapply(dataset$calculatedSteps, dataset$date, sum)


hist(StepsPerDaySum$calculatedSteps, breaks=20, xlab="Number of Steps Taken", 
     main="Frequency of Total Steps Taken",ylab = "Number of Days", col="lightblue")


p <- ggplot(StepsPerDaySum, aes(x = calculatedSteps))
p + geom_histogram(binwidth=1100) + labs(title = "Frequency of Total Steps Taken", x= "Number of Steps Taken", y="Number of Days")
 
mean(StepsPerDaySum$calculatedSteps)
median(StepsPerDaySum$calculatedSteps)


#Are there differences in activity patterns between weekdays and week- ends?
#For this part the weekdays() function may be of some help here. Use the 
#dataset with the filled-in missing values for this part.

#1. Create a new factor variable in the dataset with two levels – “weekday” and 
  #“weekend” indicating whether a given date is a weekday or weekend day.

#creats a variable called "weekend" indicated whether the day is a weekend or not. (TRUE, FALSE)
for (i in 1: length(dataset$DateTime)){dataset$weekend[i] <- wday(dataset$DateTime[i]) %in% c(1,7)}

# Two new datasets one for the weekday and one for the weekend.
WeekendDataset <- filter(dataset, weekend == TRUE)
WeekdayDataset <- filter(dataset, weekend == FALSE)


#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
  #interval (x-axis) and the average number of steps taken, averaged across all 
  #weekday days or weekend days (y-axis). The plot should look something like the 
  #following, which was creating using simulated data:


# Mean steps per interval Weekday

StepsPerIntervalMean$WeekdayCalculatedSteps <-tapply(WeekdayDataset$calculatedSteps, WeekdayDataset$Time, mean)

# Mean steps per interval Weekend

StepsPerIntervalMean$WeekendCalculatedSteps <-tapply(WeekendDataset$calculatedSteps, WeekendDataset$Time, mean)





# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



### Weekday Plot
p1 <- ggplot(StepsPerIntervalMean, aes(x=HourDecimalMin, y=WeekdayCalculatedSteps, group=1))
p1 <- p1 + geom_line() + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps in 5 Minute Intervals During Weekdays From Midnight To Midnight.")

#### Weekend Plot

p2 <- ggplot(StepsPerIntervalMean, aes(x=HourDecimalMin, y=WeekendCalculatedSteps, group=1))
p2 <- p2 + geom_line() + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps in 5 Minute Intervals During Weekends From Midnight To Midnight.")

#### Use multiplot to make 2 panels 
multiplot(p1, p2, cols=1)





