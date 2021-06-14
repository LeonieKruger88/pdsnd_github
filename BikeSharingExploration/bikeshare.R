#Install and load equired libraries
install.packages("lubridate")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("janitor")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(tidyr)
library(janitor)
library(ggplot2)

createDateColumns <- function(d) {
  #Create a date column. This will support working with dates and date analysis
  d$Start.Date = as.Date(d$Start.Time)
  
  #Separate date time into month, years and days. This will support analysis on days, months and years
  d=d%>%
    separate("Start.Date",into=c("Start.Year","Start.Month","Start.Day"),sep="-",remove = FALSE)
  
  #Add a column for weekday
  d$Start.Weekday=factor(weekdays(d$Start.Date))
  
  return(d)
}

monthlyRentalAnalysis <- function() {
  #Read datasets
  ny=read.csv('new-york-city.csv')
  wash=read.csv('washington.csv')
  chi=read.csv('chicago.csv')
  
  #Create date columns
  ny=createDateColumns(ny)
  wash=createDateColumns(wash)
  chi=createDateColumns(chi)


  #Create columns in Washington dataset that are in the other datasets, but missing from Washington
  wash$Gender= NA
  wash$`Birth.Year`=NA

  #Create a flag that can be used to identify the stations
  ny$StationName='NY'
  wash$StationName='WASH'
  chi$StationName='CHI'

  #Bind the datasets together
  allBookings= rbind(ny,wash,chi)

  #Show the bookings for each month and add a layer for each station
  qplot(x=Start.Month, data=allBookings,
        xlab='Rental Month',
        ylab='Number of Rentals',
        main = 'Rentals per Month and Station',
        fill =Start.Month)+
    facet_wrap(~StationName) +
    theme(plot.title = element_text(hjust = 0.5))
}


weeklyRentalAnalysis <- function() {
  #Read datasets
  ny=read.csv('new-york-city.csv')
  wash=read.csv('washington.csv')
  chi=read.csv('chicago.csv')
  
  #Create date columns
  ny=createDateColumns(ny)
  wash=createDateColumns(wash)
  chi=createDateColumns(chi)
  
  
  #Create columns in Washington dataset that are in the other datasets, but missing from Washington
  wash$Gender= NA
  wash$`Birth.Year`=NA
  
  #Create a flag that can be used to identify the stations
  ny$StationName='NY'
  wash$StationName='WASH'
  chi$StationName='CHI'
  
  #Bind the datasets togethr
  allBookings= rbind(ny,wash,chi)

  #Order by weekday
  allBookings$Start.Weekday <- factor(allBookings$Start.Weekday, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  allBookings[order(allBookings$Start.Weekday), ]

  #Show the bookings for each month and day of week for each station
  qplot(x=Start.Weekday, data=allBookings,
        xlab='Rental Week Day',
        ylab='Number of Rentals',
        main = 'Weekday Rentals per Month and Station',
        fill =Start.Weekday)+
   facet_grid(StationName~Start.Month)+
    theme(plot.title = element_text(hjust = 0.5))
}

tripDurationAgeAnalysis <- function() {
  #Read datasets
  ny=read.csv('new-york-city.csv')
  wash=read.csv('washington.csv')
  chi=read.csv('chicago.csv')
  
  #Add an age column. This will be the current year minus the birth year
  chi = chi %>%
    mutate(age=as.numeric(format(Sys.Date(), format="%Y"))-`Birth.Year`)

  summary(chi$age)

  #Plot trip duration per age group. Also add mean and quantiles
  ggplot(aes(x=age,y=Trip.Duration),data=subset(chi,!is.na(age)))+
    geom_jitter(alpha=1/20,
                color='red')+
    xlim(20,72)+
    coord_cartesian(ylim   = c(0,1000)) +
    stat_summary(geom = "line", fun = "mean",  size = 1) +
    stat_summary(geom = "line", fun = "quantile", fun.args = list(probs = .1), color = "blue",linetype = 2)+
    stat_summary(geom = "line", fun = "quantile", fun.args = list(probs = .75), color = "blue")+
    ggtitle("Trip duration per age group")+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x = "Customer Age",
         y = "Trip Duration") 
}
  
customerTypeAgeAnalysis <- function() {
  #Read datasets
  ny=read.csv('new-york-city.csv')
  wash=read.csv('washington.csv')
  chi=read.csv('chicago.csv')
  
  #Add an age column. This will be the current year minus the birth year
  chi = chi %>%
    mutate(age=as.numeric(format(Sys.Date(), format="%Y"))-`Birth.Year`)

  #Get age statistics for each user type
  table(chi$User.Type)
  by(chi$age,chi$User.Type,summary)

  #Set upper and lower limit to exclude the outliers from the data as this is less representative of the groups
  qplot(User.Type,
        age,data=subset(chi,!is.na(age)),
        geom='boxplot',
        main = 'Age of Customer Types')+
    coord_cartesian(ylim   = c(30,55))+
    labs(x = "Customer Type",
         y = "Customer Age") +
    theme(plot.title = element_text(hjust = 0.5))

  by(chi$age,chi$User.Type,summary)
}



