library(lubridate)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggthemes)
library(gridExtra)

#Reading dataset
energy_src <- read.csv("/Users/jenni/OneDrive/Desktop/Energy_consumption/energydata_complete.csv")
str(energy_src)

#Converting date from Factor to POSIXct
energy_src$date <- strptime(as.character(energy_src$date),format="%Y-%m-%d %H:%M:%S")
energy_src$date <- as.POSIXct(energy_src$date,tz = "EST")
class(energy_src$date)
str(energy_src)

#Define a function to set Weekday and Weekend
weekend_weekday <- function(x) {
  val <- weekdays(x)
  if (val == "Saturday" | val == "Sunday") {
    val2 = "Weekend"
  }
  else {
    val2= "Weekday"
  }
  return(val2)
}

#Derive new columns to store Time in seconds, Day of the week and Weekend/Weekday Status
energy_src$seconds=hour(energy_src$date)*3600+minute(energy_src$date)*60+second(energy_src$date)
energy_src$WeekStatus <- unlist(lapply(energy_src$date,weekend_weekday))
energy_src$Day_of_week <-weekdays(energy_src$date)

unique(energy_src$Day_of_week)
unique(energy_src$WeekStatus)
unique(energy_src$seconds)

str(energy_src)

#Visualization

qplot(energy_src$date,energy_src$Appliances,xlab='Time',ylab='Appliances Wh',geom="line")+theme_grey(base_size = 18)
qplot(energy_src$date[1:1008],energy_src$Appliances[1:1008],xlab='Time (1 week)',ylab='Appliances Wh',geom="line")+theme_grey(base_size = 18) 

# Usage on weekdays
energy_src_weekday=energy_src %>% filter(WeekStatus =='Weekday')
qplot(energy_src_weekday$date,energy_src_weekday$Appliances,xlab='Time',ylab='Appliances Wh',geom="line")+theme_grey(base_size = 18) 

# Usage on weekend
energy_src_weekend=energy_src %>% filter(WeekStatus =='Weekend')
qplot(energy_src_weekend$date,energy_src_weekend$Appliances,xlab='Time',ylab='Appliances Wh',geom="line")+theme_grey(base_size = 18) 


hist(energy_src$Appliances,main="",xlab = "Appliances Wh",breaks = 40,
     col='lightblue',xlim=c(0,1200),ylim=c(0,9000),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

boxplot(energy_src$Appliances,
        boxfill = "lightblue",horizontal=TRUE,ylim=c(0,1200),xlab="Appliances Wh",frame=F,
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


# HEAT MAP visualization
# Visualization of the Energy use per week with heat map


energy_src$my <- floor_date(energy_src$date,"month")
energy_src$mhr <- floor_date(energy_src$date,"hour")
energy_src_Total_per_hour <-  ddply(energy_src, "mhr", summarise,
                                     Appliances=sum(Appliances))
energy_src_Total_per_hour
energy_src_Total_per_hour$Day_week <- wday(energy_src_Total_per_hour$mhr,label=TRUE)
class(energy_src_Total_per_hour)
summary(energy_src_Total_per_hour)
energy_src_Total_per_hour_na_removed <- na.omit(energy_src_Total_per_hour)

dim(energy_src_Total_per_hour)
names(energy_src_Total_per_hour)
dim(energy_src_Total_per_hour_na_removed)
names(energy_src_Total_per_hour_na_removed)
summary(energy_src_Total_per_hour_na_removed)

# getting now the week of the year
energy_src_Total_per_hour_na_removed$week_year <- week(energy_src_Total_per_hour_na_removed$mhr)
head(energy_src_Total_per_hour_na_removed)

unique(energy_src_Total_per_hour_na_removed$week_year)


# Third week only
energy_src_Total_per_hour_na_removed_w1 <- energy_src_Total_per_hour_na_removed[energy_src_Total_per_hour_na_removed$week_year ==3,]
energy_src_Total_per_hour_na_removed_w1
energy_src_Total_per_hour_na_removed_w1$Hour <- hour(energy_src_Total_per_hour_na_removed_w1$mhr)
names(energy_src_Total_per_hour_na_removed_w1)

gg1 <-ggplot(energy_src_Total_per_hour_na_removed_w1,aes(x=Day_week,y=Hour,
                                                          fill=Appliances)) 
gg1 <- gg1 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg1 <- gg1 +scale_y_continuous(breaks=seq(0,23,1)) 
gg1 <- gg1 + coord_equal()
gg1 <- gg1 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg1 <- gg1 + theme_tufte(base_family="Helvetica")
gg1 <- gg1 + theme(plot.title=element_text(hjust=0))
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text=element_text(size=9))
gg1 <- gg1 + theme(legend.title=element_text(size=9))
gg1 <- gg1 + theme(legend.text=element_text(size=9))
gg1

# Fouth week only
energy_src_Total_per_hour_na_removed_w2 <- energy_src_Total_per_hour_na_removed[energy_src_Total_per_hour_na_removed$week_year ==4,]
energy_src_Total_per_hour_na_removed_w2
energy_src_Total_per_hour_na_removed_w2$Hour <- hour(energy_src_Total_per_hour_na_removed_w2$mhr)
names(energy_src_Total_per_hour_na_removed_w2)

gg2 <-ggplot(energy_src_Total_per_hour_na_removed_w2,aes(x=Day_week,y=Hour,
                                                         fill=Appliances))
gg2 <- gg2 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg2 <- gg2 +scale_y_continuous(breaks=seq(0,23,1)) 
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg2 <- gg2 + theme_tufte(base_family="Helvetica")
gg2 <- gg2 + theme(plot.title=element_text(hjust=0))
gg2 <- gg2 + theme(axis.ticks=element_blank())
gg2 <- gg2 + theme(axis.text=element_text(size=9))
gg2 <- gg2 + theme(legend.title=element_text(size=9))
gg2 <- gg2 + theme(legend.text=element_text(size=9))
gg2

# Fifth week only
energy_src_Total_per_hour_na_removed_w3 <- energy_src_Total_per_hour_na_removed[energy_src_Total_per_hour_na_removed$week_year ==5,]
energy_src_Total_per_hour_na_removed_w3
energy_src_Total_per_hour_na_removed_w3$Hour <- hour(energy_src_Total_per_hour_na_removed_w3$mhr)
names(energy_src_Total_per_hour_na_removed_w3)

gg3 <-ggplot(energy_src_Total_per_hour_na_removed_w3,aes(x=Day_week,y=Hour,
                                                         fill=Appliances))
gg3 <- gg3 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg3 <- gg3 +scale_y_continuous(breaks=seq(0,23,1)) 
gg3 <- gg3 + coord_equal()
gg3 <- gg3 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg3 <- gg3 + theme_tufte(base_family="Helvetica")
gg3 <- gg3 + theme(plot.title=element_text(hjust=0))
gg3 <- gg3 + theme(axis.ticks=element_blank())
gg3 <- gg3 + theme(axis.text=element_text(size=9))
gg3 <- gg3 + theme(legend.title=element_text(size=9))
gg3 <- gg3 + theme(legend.text=element_text(size=9))
gg3

# Sixth week only
energy_src_Total_per_hour_na_removed_w4 <- energy_src_Total_per_hour_na_removed[energy_src_Total_per_hour_na_removed$week_year ==6,]
energy_src_Total_per_hour_na_removed_w4
energy_src_Total_per_hour_na_removed_w4$Hour <- hour(energy_src_Total_per_hour_na_removed_w4$mhr)
names(energy_src_Total_per_hour_na_removed_w4)

gg4 <-ggplot(energy_src_Total_per_hour_na_removed_w4,aes(x=Day_week,y=Hour,
                                                         fill=Appliances))
gg4 <- gg4 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg4 <- gg4 +scale_y_continuous(breaks=seq(0,23,1)) 
gg4 <- gg4 + coord_equal()
gg4 <- gg4 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg4 <- gg4 + theme_tufte(base_family="Helvetica")
gg4 <- gg4 + theme(plot.title=element_text(hjust=0))
gg4 <- gg4 + theme(axis.ticks=element_blank())
gg4 <- gg4 + theme(axis.text=element_text(size=9))
gg4 <- gg4 + theme(legend.title=element_text(size=9))
gg4 <- gg4 + theme(legend.text=element_text(size=9))
gg4

g <- grid.arrange(gg1, gg2,gg3, gg4,ncol=4)
g















