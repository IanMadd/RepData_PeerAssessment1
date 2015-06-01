# Reproducible Research: Peer Assessment 1
<br>

**Description**

This project analyses data recorded from a fitness tracker.
The [data][1] shows the number of steps take by one individual over the course of 61 days. The data records three variables, the date, the number of steps, and the time in 5 minute intervals.

[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

## Loading and preprocessing the data

The first step is to load the libraries:

```r
library(data.table) 
library(knitr) 
library(lubridate) 
library(ggplot2) 
library(dplyr) 
```
<br>


Then read the data into R and create a data.table called "dataset". 


```r
dataset <- data.table(read.csv("activity.csv"))
```



```r
names(dataset)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(dataset)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
There are three columns, steps, date, and interval. A fourth variable called DateTime, combines the Date column with the interval column. This variable has a POSIXct class. Then a Time column is added which is a character vector indicating the time of day. 


```r
dataset <- cbind(dataset, DateTime = as.POSIXct(paste(dataset$date, 
        sprintf("%04d", dataset$interval)), "%Y-%m-%d %H%M", tz = "UTC"))
dataset$Time <- strftime(dataset$DateTime, format="%H:%M:%S", tz="UTC")
```

<br>
<br>


## What is mean total number of steps taken per day?

<br>

The StepsPerDaySum shows the total number of steps taken on each day. This dataset is calculated using tapply.

```r
StepsPerDaySum <- tapply(dataset$steps, dataset$date, sum)
StepsPerDaySum <- data.table(Date = as.Date(names(StepsPerDaySum)), 
Steps = StepsPerDaySum)
```

<br>

```r
head(StepsPerDaySum)
```

```
##         Date Steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


<br>

**Mean and Median Steps / Day**

```r
mean(as.numeric(StepsPerDaySum$Steps), na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(as.numeric(StepsPerDaySum$Steps), na.rm=TRUE)
```

```
## [1] 10765
```

<br>

####Histogram of the frequency of total daily steps:

<br>

This histogram shows the frequency of the number total steps per day. 


```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
p <- ggplot(StepsPerDaySum, aes(x = Steps))
p + geom_histogram(binwidth=1100) + labs(y = "Count", title="Frequency of Steps Taken Per Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />


## What is the average daily activity pattern?
<br>

The StepsPerIntervalMean data.table calculates the mean number of steps taken for each 5 minute time period across all days in the dataset. 

```r
StepsPerIntervalMean <- tapply(dataset$steps, dataset$Time, mean, na.rm=TRUE)
StepsPerIntervalMean <- data.table(Time = names(StepsPerIntervalMean), Steps = StepsPerIntervalMean)
StepsPerIntervalMean$HourDecimalMin <- as.numeric(levels(as.factor(hour(dataset$DateTime) + minute(dataset$DateTime)/60)))
StepsPerIntervalMean$Interval <- dataset$interval[1:288]
```




```r
p <- ggplot(StepsPerIntervalMean, aes(x=HourDecimalMin, y=Steps, group=1))
p + geom_line() + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps in 5 minute intervals from midnight to midnight.")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" title="" alt="" style="display: block; margin: auto;" />


The 8:35 AM time interval is the time with the greatest number of mean steps.


```r
StepsPerIntervalMean[StepsPerIntervalMean$Steps==max(StepsPerIntervalMean$Steps)] 
```

```
##        Time    Steps HourDecimalMin Interval
## 1: 08:35:00 206.1698       8.583333      835
```



## Imputing missing values
<br>

There are a number of missing values for the steps variable in the original data set.

```r
table(is.na(dataset$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```
<br>

This creates a new variable "calculatedSteps" which is equal to the original number of steps if that number is available, or if that value is NA, the calculatedSteps value is the mean of all steps for that interval.  

```r
for (i in 1:length(dataset$steps)){
  if (is.na(dataset$steps[i])) {
    dataset$calculatedSteps[i] <- StepsPerIntervalMean$Steps[StepsPerIntervalMean$Interval == dataset$interval[i]] 
  }else{
    dataset$calculatedSteps[i] <- dataset$steps[i]
  }
}
```
<br>

Use tapply to create a new StepsPerDaySum variable called "calculatedSteps" which shows the total number of steps each day based on the new calculatedSteps variable in the dataset.

```r
StepsPerDaySum$calculatedSteps <- tapply(dataset$calculatedSteps, dataset$date, sum)
```
<br>


This shows the frequency of total steps take per day. Notice that there are a significantly greater number of days grouped around the mean or median of total steps per day. 

```r
p <- ggplot(StepsPerDaySum, aes(x = calculatedSteps))
p + geom_histogram(binwidth=1100) + labs(title = "Frequency of Total Steps Taken", x= "Number of Steps Taken", y="Number of Days")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-16-1.png" title="" alt="" style="display: block; margin: auto;" />
<br>

This is confirmed with a quick calculation showing the number of days within the binwidth (1100) of the median of both variables.


```r
sum((StepsPerDaySum$Steps>=10215) & (StepsPerDaySum$Steps<=11315), na.rm=T)
```

```
## [1] 8
```

```r
sum((StepsPerDaySum$calculatedSteps>=10215) & (StepsPerDaySum$calculatedSteps<=11315), na.rm=T)
```

```
## [1] 16
```
<br>

The standard deviation of both data variables shows that the calculatedSteps is smaller than the original Steps variable.

```r
sd(StepsPerDaySum$Steps, na.rm = TRUE)
```

```
## [1] 4269.18
```

```r
sd(StepsPerDaySum$calculatedSteps)
```

```
## [1] 3974.391
```
<br>

And the mean and median basically haven't changed for, what should be, obvious reasons.

```r
mean(StepsPerDaySum$calculatedSteps); median(StepsPerDaySum$calculatedSteps)
```

```
## [1] 10766.19
```

```
## [1] 10766.19
```

<br>
<br>

## Are there differences in activity patterns between weekdays and weekends?

<br>

Using the dplyr command "wday" I can create a new variable in the original dataset called "weekend" which indicates "TRUE" if the day is a weekend and "FALSE" if it is a weekday.

```r
for (i in 1: length(dataset$DateTime)){dataset$weekend[i] <- wday(dataset$DateTime[i]) %in% c(1,7)}
```
<br>

Two new datasets are created by filtering the original dataset into weekday or weekend datasets.

```r
# Two new datasets one for the weekday and one for the weekend.
WeekendDataset <- filter(dataset, weekend == TRUE)
WeekdayDataset <- filter(dataset, weekend == FALSE)
```
<br>

Then using tapply, two new variables are created in the StepsPerIntervalMean dataset showing the mean number of steps for each interval 


```r
# Mean steps per interval Weekday
StepsPerIntervalMean$WeekdayCalculatedSteps <-tapply(WeekdayDataset$calculatedSteps, WeekdayDataset$Time, mean)

# Mean steps per interval Weekend
StepsPerIntervalMean$WeekendCalculatedSteps <-tapply(WeekendDataset$calculatedSteps, WeekendDataset$Time, mean)
```


<br>

Initialize the Multiplot Function which displays multiple unrelated plots together using ggplot2.



```r
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
```

<br>



```r
### Weekday Plot
p1 <- ggplot(StepsPerIntervalMean, aes(x=HourDecimalMin, y=WeekdayCalculatedSteps, group=1))
p1 <- p1 + geom_line() + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps During Weekdays From Midnight To Midnight.")

#### Weekend Plot

p2 <- ggplot(StepsPerIntervalMean, aes(x=HourDecimalMin, y=WeekendCalculatedSteps, group=1))
p2 <- p2 + geom_line() + scale_x_continuous(breaks=0:24) + labs(x = "Hours", y = "Mean Steps", title= "Mean Steps During Weekends From Midnight To Midnight.")
```

<br>

The two plots below show the average activity patterns of weekdays versus weekends.


```r
#### Use multiplot to make 2 panels 
multiplot(p1, p2, cols=1)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-25-1.png" title="" alt="" style="display: block; margin: auto;" />

<br>

The data shows more activity on weekdays between 8 AM and 9 AM but more activity during the day on weekends. This is confirmed by quickly calculating the mean of steps recorded on weekdays and weekends.


```r
mean(WeekdayDataset$calculatedSteps)
```

```
## [1] 35.61058
```

```r
mean(WeekendDataset$calculatedSteps)
```

```
## [1] 42.3664
```


<br>
<br>






