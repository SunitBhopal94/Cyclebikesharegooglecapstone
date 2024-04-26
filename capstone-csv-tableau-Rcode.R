#-------------------------------------Creating csv file for Tableau------------------------------------------

#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#load original .csv files, a years worth of data from jan 2023 to dec 2023
rides_jan23<-read.csv("~/R_studio/202301-divvy-tripdata.csv")
rides_feb23<-read.csv("~/R_studio/202302-divvy-tripdata.csv")
rides_mar23<-read.csv("~/R_studio/202303-divvy-tripdata.csv")
rides_apr23<-read.csv("~/R_studio/202304-divvy-tripdata.csv")
rides_may23<-read.csv("~/R_studio/202305-divvy-tripdata.csv")
#View(rides_may23)
rides_jun23<-read.csv("~/R_studio/202306-divvy-tripdata.csv")
rides_jul23<-read.csv("~/R_studio/202307-divvy-tripdata.csv")
rides_aug23<-read.csv("~/R_studio/202308-divvy-tripdata.csv")
rides_sep23<-read.csv("~/R_studio/202309-divvy-tripdata.csv")
rides_oct23<-read.csv("~/R_studio/202310-divvy-tripdata.csv")
rides_nov23<-read.csv("~/R_studio/202311-divvy-tripdata.csv")
rides_dec23<-read.csv("~/R_studio/202312-divvy-tripdata.csv")

#merge all of the data frames into one year view
rides2023_df<-rbind(rides_jan23,rides_feb23,rides_mar23,rides_apr23,rides_may23,rides_jun23,rides_jul23,rides_aug23,rides_sep23,rides_oct23,rides_nov23,rides_dec23)

#remove individual month data frames to clear up space in the environment 
remove(rides_jan23,rides_feb23,rides_mar23,rides_apr23,rides_may23,rides_jun23,rides_jul23,rides_aug23,rides_sep23,rides_oct23,rides_nov2,rides_dec23)

#create new data frame to contain new columns
rides2023_date <- rides2023_df_v3

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
rides2023_date$ride_length <- difftime(rides2023_df_v3$ended_at, rides2023_df_v3$started_at, units = "mins")

#create columnds for: day of week, month, day, year, time, hour
rides2023_date$date <- as.Date(rides2023_date$started_at) #default format is yyyy-mm-dd, use start date
rides2023_date$day_of_week <- wday(rides2023_df_v3$started_at) #calculate the day of the week 
rides2023_date$day_of_week <- format(as.Date(rides2023_date$date), "%A") #create column for day of week
rides2023_date$month <- format(as.Date(rides2023_date$date), "%m")#create column for month
rides2023_date$day <- format(as.Date(rides2023_date$date), "%d") #create column for day
rides2023_date$year <- format(as.Date(rides2023_date$date), "%Y") #create column for year
rides2023_date$time <- format(as.Date(rides2023_date$date), "%H:%M:%S") #format time as HH:MM:SS
rides2023_date$time <- as_hms((rides2023_df_v3$started_at)) #create new column for time
rides2023_date$hour <- hour(rides2023_date$time) #create new column for hour

#create column for different seasons: Spring, Summer, Fall, Winter
rides2023_date <-rides2023_date %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
rides2023_date <-rides2023_date %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)


#clean the data
rides2023_date <- na.omit(rides2023_date) #remove rows with NA values
rides2023_date <- distinct(rides2023_date) #remove duplicate rows 
rides2023_date <- rides2023_date[!(rides2023_date$ride_length <=0),] #remove where ride_length is 0 or negative
rides2023_date <- rides2023_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(rides2023_date)
#created a new dataframe to use in Tableau
rides2023_df_tableau <- rides2023_date

#clean the data
rides2023_df_tableau  <- rides2023_df_tableau  %>%  #remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

#download the new data as a .csv file
fwrite(rides2023_df_tableau ,"rides2023_df_tableau _data.csv")