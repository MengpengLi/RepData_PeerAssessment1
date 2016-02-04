activity <- read.csv("activity.csv",colClasses = c("numeric","POSIXct","numeric"))
steps <- data.frame(tapply(activity$steps,activity$date, sum))
steps <- cbind(as.Date(row.names(steps)),steps)
colnames(steps) = c("date","sum")
plot(steps$date,steps$sum,type = "h")

series <- subset(activity, !is.na(activity$steps))
series <- data.frame(tapply(series$steps, series$interval, mean))
series <- cbind(as.numeric(row.names(series)),series)
colnames(series) <- c("interval","mean")
plot(series$interval,series$mean, type = "l")
maxInterval <- which.max(series$mean)

countNA <- table(is.na(activity$steps))[2]
Filled <- activity
for(i in 1:length(Filled$steps)){
      if(is.na(Filled$steps[i])){
            temp <- which(Filled$interval[i] == series$interval)
            Filled[i,1] <- as.numeric(series[temp,2])
      }
}
steps2 <- data.frame(tapply(Filled$steps,Filled$date, sum))
steps2 <- cbind(as.Date(row.names(steps2)),steps2)
colnames(steps2) = c("date","sum")
plot(steps2$date,steps2$sum,type = "h")

