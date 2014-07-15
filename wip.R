## DATA LOADING AND PREPARATION
## read file from working directory
print("Reading and preparing data...")
data <- read.csv("activity.csv",
                 na.strings = "NA",
                 header=TRUE,
                 stringsAsFactors = FALSE,
                 nrows=17568+1)

require("stringr") ## required for str_sub function

## transform interval column in hh:mm time

# x <- paste("000",as.character(data$interval),sep="")
# x <- paste(str_sub(x,start=-4L,end=-3L), ":", str_sub(x,start=-2L), ":00", sep="")
#
# ## convert date to date format in new column
# data$DateTime <- strptime(paste(data$date, x), "%F %T")
## DONE: DATA LOADING AND PREPARATION

par(mfcol = c(2,2))

################## What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
print("Studying steps per day...")
data.noNA<-data[!is.na(data$steps),]
steps.per.day<-tapply(data.noNA$steps,data.noNA$date,FUN=sum)

# Make a histogram of the total number of steps taken each day
hist(x=steps.per.day,
     col="RED",
     cex.axis=0.8,
     cex.lab=0.8,
     cex.main=0.9,
     main="Steps per day frequency",
     xlab="Number of steps")

# Calculate and report the mean and median total number of steps taken per day
v<-median(steps.per.day)
abline(v=v,col="blue")
text(v+4000,28.7,labels=paste("Median= ",round(v)),col="blue", cex=0.8)

m<-mean(steps.per.day)
points(x=m, y=0,col="green",lwd=5)
text(m+4000,1,labels=paste("Mean= ",round(m)),col="green", cex=0.8)


################

################# What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

print("Studying steps per interval...")

## compute values (average per interval)
steps.per.interval<-tapply(data.noNA$steps,data.noNA$interval,FUN=mean)
## draw main plot
plot(x=dimnames(steps.per.interval)[[1]],
     y=steps.per.interval,
     xlab="5-minute interval",
     ylab="Number of steps",
     type="l",
     main="Steps per 5-minute interval")

## compute max coordinates
ymax<-max(steps.per.interval)
xmax<-names(steps.per.interval[which(steps.per.interval==ymax)])
## add green point
points(x=xmax,
       y=ymax,
       col="green",
       cex=2,
       pch=1,
       lwd=2)
## add label for green point
x <- paste("000",xmax,sep="")
x <- paste(str_sub(x,start=-4L,end=-3L), ":", str_sub(x,start=-2L), sep="")
text(x=as.numeric(xmax)+700,
     y=ymax,
     paste("max:",ymax,"steps @",x),
     col="green",
     cex=0.8)

## save to png
dev.copy(png, file = "figures/stepsperinterval.png")  ## Copy my plot to a PNG file
dev.off()                          ## Don't forget to close the PNG device!


## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print(paste("Number of rows with NA steps:",sum(is.na(data$steps))))

## replace missing values (NA) by mean of corresponding steps per interval
data.completed <- data
steps.per.interval<-data.frame(steps.per.interval)
steps.per.interval<-cbind(steps.per.interval,rownames(steps.per.interval))
names(steps.per.interval) <- c("steps","interval")

steps.is.na <- is.na(data.completed$steps)
for (i in steps.per.interval$interval) {
        ok <- data.completed$interval == i & steps.is.na
        a <- steps.per.interval[steps.per.interval$interval==i,]$steps
        data.completed[ok, "steps"] <- a
}

steps.per.day.completed <-tapply(data.completed$steps,data.completed$date,FUN=sum)

# Make a histogram of the total number of steps taken each day
hist(x=steps.per.day.completed,
     col="RED",
     cex.axis=0.8,
     cex.lab=0.8,
     cex.main=0.9,
     main="Steps per day frequency \n (NA values estimated)",
     xlab="Number of steps")

# Calculate and report the mean and median total number of steps taken per day
v<-median(steps.per.day.completed)
abline(v=v,col="blue")
text(v+4000,28.7,labels=paste("Median= ",round(v)),col="blue", cex=0.8)

m<-mean(steps.per.day.completed)
points(x=m, y=0,col="green",lwd=5)
text(m+4000,1,labels=paste("Mean= ",round(m)),col="green", cex=0.8)

## weekends vs weekdays
## as i am using a non english system, i have to set the locale so that days of
## the week are in english
## i think this is OS dependent so it may not work on your OS
Sys.setlocale(category = "LC_TIME", locale="English_US.1252")
day <- weekdays(strptime(data.completed$date, "%Y-%m-%d"))
data.completed$weekend <- day %in% c("Saturday", "Sunday")
data.completed$weekday <- !data.completed$weekend

################################################################
# ## Plot 1
# ## draw histogram with color and labels
# hist(data$Global_active_power,
#      col="RED",
#      cex.axis=0.8,
#      cex.lab=0.8,
#      cex.main=0.9,
#      main="Global Active Power",
#      xlab="Global Active Power (kilowatts)")
#
# ## save the image of the plot
# dev.copy(png, file = "plot1.png")  ## Copy my plot to a PNG file
# dev.off()                          ## Don't forget to close the PNG device!
