## The dataset can be downloaded from the link below
## https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
data <- read.table(unz("activity.zip", "activity.csv"), sep = ",", header = TRUE)
data$IsWeekend <- weekdays(as.Date(data$date)) %in% c("Saturday", "Sunday")
data$IsWeekend <- factor(data$IsWeekend, labels = c("Weekday", "Weekend"))


totalStepsPerDay <- aggregate(data$steps, by = list(data$date), FUN = sum)
names(totalStepsPerDay) <- c("date", "steps")
hist(totalStepsPerDay$steps, breaks = 20, ylim = range(0, 10),
     col = "gray", main = "Histogram of Steps taken per day", xlab = "Steps")
meanStepsPerDay = mean(totalStepsPerDay$steps, na.rm = TRUE)
medianStepsPerDay = median(totalStepsPerDay$steps, na.rm = TRUE)


cleanData <- data[!is.na(data$steps), ]
stepsPer5MinInterval <- aggregate(cleanData$steps, by = list(cleanData$interval), FUN = mean)
names(stepsPer5MinInterval) <- c("interval", "steps")
with(stepsPer5MinInterval, plot(interval, steps, type = "l", main = "Average # of Steps per interval",
                                xlab = "Interval", ylab = "Steps", col = "blue"))
maxSteps <- max(stepsPer5MinInterval$steps)
maxStepsInterval <- stepsPer5MinInterval[stepsPer5MinInterval$steps == maxSteps, ]$interval


numOfNARows <- nrow(data[is.na(data$steps), ])
percentageOfNARows <- numOfNARows / nrow(data)
newData <- data
naRows <- is.na(newData$steps)
for (i in 1:nrow(newData[naRows, ])) {
    interval <- newData[naRows, ][i, "interval"]
    date <- newData[naRows, ][i, "date"]
    steps <- stepsPer5MinInterval[stepsPer5MinInterval$interval == interval, "steps"]
    if (length(steps) != 0){
        newData[naRows, ][i, "steps"] <-
            stepsPer5MinInterval[stepsPer5MinInterval$interval == interval, "steps"]
    } else {
        newData[naRows, ][i, "steps"] <- mean(cleanData[cleanData$date == date, ]$steps)
    }
}
newTotalStepsPerDay <- aggregate(newData$steps, by = list(newData$date), FUN = sum)
names(newTotalStepsPerDay) <- c("date", "steps")
hist(newTotalStepsPerDay$steps, breaks = 20, ylim = range(0, 20),
     col = "gray", main = "Histogram of Steps taken per day", xlab = "Steps")
newMeanStepsPerDay = mean(newTotalStepsPerDay$steps, na.rm = TRUE)
newMedianStepsPerDay = median(newTotalStepsPerDay$steps, na.rm = TRUE)

newStepsPer5MinInterval <- aggregate(newData$steps, by = list(newData$interval, newData$IsWeekend), FUN = mean)
names(newStepsPer5MinInterval) <- c("interval", "IsWeekend", "steps")
library(lattice)
xyplot(steps ~ interval | IsWeekend, data = newStepsPer5MinInterval, type = "l",
       xlab = "Interval", ylab = "Number of Steps", layout = c(1, 2))