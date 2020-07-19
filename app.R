#Install these packages if not already installed
#install.packages("shiny")
#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(proxy)
library(lubridate)
library(ggplot2)
library(plyr)
library(viridis)
library(ggthemes)
library(caret)
#library(corrplot)
#library(Hmisc)
library(PerformanceAnalytics)
library(dplyr)
library(acnr)
library(forecast)
library(urca)
library(vars)



#Reading dataset
#energy_src <- read.csv("/Users/vivekmuralidharan/Documents/BigDataAnalytics2/Course 2 - 1040/Project3/energydata_complete.csv")
energy_src <- read.csv("energydata_complete.csv")

#Converting date from Factor to POSIXct
energy_src$date <- strptime(as.character(energy_src$date),format="%Y-%m-%d %H:%M:%S")
energy_src$date <- as.POSIXct(energy_src$date,tz = "EST")


################################Model######################

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

#Model

dat.bv <- cbind(Appliances, Tmean, RHmean, T_out, RH_out)
colnames(dat.bv) <- c("Appliances", "Tmean", "RHmean", "T_out", "RH_out")

bv.est <- VAR(dat.bv, p = 1, type = "const", season = NULL, 
              exog = NULL)




# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Select Input"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("slider",
                  "Number of days to predict:",
                  value = 5,
                  min = 1,
                  max = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
              tabPanel("Forecast", plotOutput("Forecast")
      )
      
    )
  )
)

EnergyForecast <- function(input) {
  predictions <- predict(bv.est, n.ahead = input, ci = 0.95)
  return(predictions)
}


shinyServer <- function(input, output) {

  #Forecast
  
  output$Forecast <- renderPlot({
    fanchart(EnergyForecast(input$slider), names="Appliances", main="Forecasted Energy Usage")
  })
}

#Create Shiny object

shinyApp(ui = ui, server = shinyServer) #Shiny call
