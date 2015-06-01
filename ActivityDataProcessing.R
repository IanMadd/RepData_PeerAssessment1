library(knitr)
library(data.table)

unzip("activity.zip")
Activity_Data <- data.table(read.csv("activity.csv"))