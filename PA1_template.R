setwd("/Users/michellecipullo/Desktop/Coursera_DataScience/ReproducibleResearch/Assignment1/")

data<-read.csv("activity.csv",colClasses = c("integer","Date","integer")) #steps (integer), date (date), interval (integer)

#1.Calculate the number of steps taken per day
numSteps<-aggregate(data$steps,by=list(data$date),FUN=sum)

numSteps.NoMissing<-na.omit(numSteps)
colnames(numSteps.NoMissing)=c("Date","steps")

#2. Make a HISTOGRAM of the number of steps
hist(numSteps.NoMissing$steps,xlab="number of steps",main="Total Steps taken per day")
dev.off()
#3. Calculate and report the mean and median steps per day
meanSteps<-mean(numSteps.NoMissing$steps) #10766.19
medianSteps<-median(numSteps.NoMissing$steps) #10765

#4. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
data.NoMissing<-na.omit(data)
Activity<-tapply(data.NoMissing$steps,data.NoMissing$interval,mean)
plot(x=names(Activity),y=Activity,type="l",xlab="5 minute interval",ylab="Average number of Steps",main="Averages across all days")
dev.off()
#5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Max5minInt<-Activity[which.max(Activity)] #835 206.1698

#6. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
sum(is.na(data$steps)) #2304

#7. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
data$steps[which(is.na(data$steps))]<-mean(data$steps,na.rm = TRUE)

#8. Create a new dataset that is equal to the original dataset but with the missing data filled in.
cleanData<-data #already cleaned above, so call it new name.
sum(is.na(cleanData)) #0. So fixed. 
#9. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
CleanActivity<-tapply(cleanData$steps,cleanData$date,sum)
 #plot side by side
par(mfrow=c(1,2))
hist(numSteps.NoMissing$steps,xlab="number of steps",main="Total Steps taken per day",ylim=c(0,50))
hist(CleanActivity,xlab="number of steps",main="Total Steps taken per day (imputed missing values to mean)",ylim=c(0,50))
dev.off()

#calc mean & median and compare
mean(numSteps.NoMissing$steps) #10766.19
mean(CleanActivity) #10766.19

median(numSteps.NoMissing$steps)#10765
median(CleanActivity) #10766.19

#No change in mean, slight change in median.

#10. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
cleanData$weekdays<-weekdays(cleanData$date)
cleanData$dayFactor<-as.factor(c("weekend","weekday"))
cleanData[cleanData$weekdays == "Sunday" | cleanData$weekdays == "Saturday" ,5]<- factor("weekend")
cleanData[!(cleanData$weekdays == "Sunday" | cleanData$weekdays== "Saturday"),5 ]<- factor("weekday")

#11. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

#All WEEKDAY
WeekdayActivity<-cleanData[cleanData$dayFactor=="weekday",]
WeekendActivity<-cleanData[cleanData$dayFactor=="weekend",]

meanWeekday<-tapply(WeekdayActivity$steps,WeekdayActivity$interval,mean)
meanWeekend<-tapply(WeekendActivity$steps,WeekendActivity$interval,mean)
par(mfrow=c(1,2))
plot(x=names(meanWeekday),y=meanWeekday,type="l",xlab="5 min interval",ylab="average num steps",main="Weekday step patterns")
plot(x=names(meanWeekend),y=meanWeekend,type="l",xlab="5 min interval",ylab="average num steps",main="Weekend step patterns")
