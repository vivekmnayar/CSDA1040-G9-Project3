library(lubridate)
library(ggplot2)
library(plyr)
library(viridis)
library(ggthemes)
library(caret)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(dplyr)
library(acnr)
library(forecast)
library(urca)
library(vars)



#Reading dataset
energy_src <- read.csv("/Users/vivekmuralidharan/Documents/BigDataAnalytics2/Course 2 - 1040/Project3/energydata_complete.csv")
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

# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg1 <-ggplot(energy_src_Total_per_hour_na_removed_w1,aes(x=Day_week,y=Hour,
                                                          fill=Appliances)) 

gg1 <- gg1 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg1

############Visualization 2

gg1 <- gg1 +scale_y_continuous(breaks=seq(0,23,1))
gg1






################################Model######################


# Reading the training data

energy_src$my <- NULL
energy_src$mhr <- NULL
names(energy_src)




set.seed(1)
train_index <- createDataPartition(energy_src$Appliances,p=0.75,list=FALSE)

train_data <- energy_src[train_index,]
dim(train_data)
names(train_data)

class(train_data$date)
str(train_data)

train_data2 <- train_data

# creating the testing set here

test_data <- energy_src[-train_index,]
dim(test_data)
# 4932,32

testing2 <- test_data


rcorr(as.matrix(train_data2[,c(2, 3, 4, 5, 6, 7, 8, 9)]))


cor(train_data2[,c(2, 3, 4, 5, 6, 7, 8, 9)], method = c("pearson", "kendall", "spearman"))

install.packages("PerformanceAnalytics")

#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)

my_data <- train_data2[,c(2, 3, 4, 5, 6, 7, 8, 9)]
chart.Correlation(my_data, histogram=TRUE, pch=19)


############
energy_src_Total_per_hour_na_removed_weeks <- energy_src_Total_per_hour_na_removed[energy_src_Total_per_hour_na_removed$week_year <=4,]
energy_src_Total_per_hour_na_removed_weeks
energy_src_Total_per_hour_na_removed_w1$Hour <- hour(energy_src_Total_per_hour_na_removed_w1$mhr)


appliances <- ts(energy_src_Total_per_hour_na_removed_weeks$Appliances)


plot(cbind(appliances))

appliances <- ts(energy_src_Total_per_hour_na_removed_weeks$Appliances, frequency = 24)
appliances <- ts(energy_src_Total_per_hour_na_removed_weeks$Appliances, frequency = 24)
############

energy_src$Tmean = (energy_src$T1 + energy_src$T2 + energy_src$T3 + energy_src$T4 + energy_src$T5 + energy_src$T6 + energy_src$T7 + energy_src$T8 +  energy_src$T9) / 9

energy_src$RHmean = (energy_src$RH_1 + energy_src$RH_2 + energy_src$RH_3 + energy_src$RH_4 + energy_src$RH_5 + energy_src$RH_6 + energy_src$RH_7 + energy_src$RH_8 + energy_src$RH_9) / 9 

energy_src2 <- data.frame(energy_src$date, energy_src$Appliances, energy_src$Tmean, energy_src$RHmean, energy_src$T_out, energy_src$RH_out)

energy_src3 <- energy_src2 %>% rename("Date"="energy_src.date", "Appliances"="energy_src.Appliances", "Tmean"="energy_src.Tmean"
                                      , "RHmean"="energy_src.RHmean", "T_out"="energy_src.T_out", "RH_out"="energy_src.RH_out")


energy_src3$my <- floor_date(energy_src3$Date,"month")
energy_src3$mhr <- floor_date(energy_src3$Date,"hour")
energy_src3$day <- floor_date(energy_src3$Date,"day")
energy_src3_Total_per_day <-  ddply(energy_src3, "day", summarise,
                                    Appliances=sum(Appliances), Tmean=mean(Tmean), RHmean=mean(RHmean)
                                    , T_out=mean(T_out), RH_out=mean(RH_out))

energy_src3_Total_per_day <- na.omit(energy_src3_Total_per_day)

####### Time Series Data 
Appliances <- ts(energy_src3_Total_per_day$Appliances, freq = 365)
Tmean <- ts(energy_src3_Total_per_day$Tmean, freq = 365)
RHmean <- ts(energy_src3_Total_per_day$RHmean, freq = 365)
T_out <- ts(energy_src3_Total_per_day$T_out, freq = 365)
RH_out <- ts(energy_src3_Total_per_day$RH_out, freq = 365)

######Time Series plot 

plot(cbind(Appliances, Tmean, RHmean, T_out, RH_out), main = "Appliances, Tmean, RHmean, T_out and RH_out")

#######Autocorrelation

Appliances_acf <- acf(Appliances, main = "Appliances_output")
Tmean_acf <- acf(Tmean, main = "Tmean_output")
RHmean_acf <- acf(RHmean, main = "RHmean_output")
T_out_acf <- acf(T_out, main = "T_out_output")
RH_out_acf <- acf(RH_out, main = "RH_out_output")


adf_Appliances <- ur.df(Appliances, type = "trend", selectlags = "AIC")
summary(adf_Appliances)


#Model

dat.bv <- cbind(Appliances, Tmean, RHmean, T_out, RH_out)
colnames(dat.bv) <- c("Appliances", "Tmean", "RHmean", "T_out", "RH_out")

info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection

bv.est <- VAR(dat.bv, p = 1, type = "const", season = NULL, 
              exog = NULL)
summary(bv.est)




#Forecast

predictions <- predict(bv.est, n.ahead = 10, ci = 0.95)

fanchart(predictions, names = "Appliances")


