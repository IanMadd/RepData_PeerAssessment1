### Replace NAs

require(data.table)  # v1.6.6
require(gdata)       # v2.8.2
require(plyr)

### helper function that creates data.tables
create_dt <- function(nrow=5, ncol=5, propNA = 0.5){
        v <- runif(nrow * ncol)
        v[sample(seq_len(nrow*ncol), propNA * nrow*ncol)] <- NA
        data.table(matrix(v, ncol=ncol))
}

remove_na <- function(x){
        dm <- data.matrix(x)
        dm[is.na(dm)] <- 0
        data.table(dm)
}

dt <- create_dt(5, 5, 0.5)
set.seed(1)



dataset$steps[is.na(dataset$steps)]

mean(dataset$steps[dataset$HourDecimalMin == dataset[1,HourDecimalMin]], na.rm=TRUE)


mean(dataset$steps[dataset$HourDecimalMin == dataset[1,HourDecimalMin]], na.rm=T)

dataset$ProcessedSteps = ifelse(is.na(dataset$steps), 
        mean(dataset$steps[dataset$HourDecimalMin == dataset[x,HourDecimalMin]], na.rm=TRUE), 
        dataset$steps)

dataset$ProcessedSteps = ifelse(is.na(dataset$steps), TRUE, FALSE)


MeanSteps <- data.table(tapply(dataset$steps, dataset$HourDecimalMin, mean, na.rm=TRUE))



dataset$ProcessedSteps = ifelse(dataset$steps < 0), TRUE, FALSE)



mean(dataset$steps[dataset$HourDecimalMin == dataset[,HourDecimalMin]], na.rm=TRUE)




# Summarise number of movie ratings by year of movie
mry <- do.call(rbind, by(movies, round(movies$rating), function(df) {
  nums <- tapply(df$length, df$year, length)
  data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
}))

p <- ggplot(mry, aes(x=year, y=number, group=rating))
p + geom_line()


p <- ggplot(dataset, aes(x = DateTime))
