#-------------------------------------R Studio ANALYSIS------------------------------------------

#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#load original .csv files, a years worth of data from August 2020 to July 2021
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

#-----------------------------------------TOTAL RIDES--------------------------------------

#total number of rides
nrow(rides2023_date)

#-----------------MEMBER TYPE---------------------
rides2023_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

#----------------TYPE OF BIKE---------------------

#total rides by member type 
rides2023_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#total rides 
rides2023_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#-------------------HOUR--------------------------

#total rides by member type 
rides2023_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
rides2023_date %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

#----------------------TIME OF DAY-----------------------

#-----morning-------
#total rides by member type 
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
rides2023_date %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides by member type 
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
rides2023_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
rides2023_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
rides2023_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#---all times of day----
#total rides by member type 
rides2023_date %>%
  group_by(member_casual) %>% 
  count(time_of_day)

#number of rides
rides2023_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------

#total rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
rides2023_date %>%
  count(day_of_week)

#----------------DAY OF THE MONTH-----------------

#total rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
rides2023_date %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble

#---------------------MONTH-----------------------

#total rides by member type 
rides2023_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
rides2023_date %>%
  count(month) 

#--------------------SEASON-----------------------

#-----spring-------

#total rides by member type 
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
rides2023_date %>%
  filter(season == "Spring") %>% 
  count(season)

#-----summer-------

#total rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
rides2023_date %>%
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------

#total rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
rides2023_date %>%
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------

#total rides by member type
rides2023_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
rides2023_date %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
rides2023_date %>%
  group_by(season, member_casual) %>% 
  count(season)

#total rides
rides2023_date %>%
  group_by(season) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(rides2023_date$ride_length)
print(cyclistic_avgRide)

#------------------MEMBER TYPE--------------------

#average ride_length
rides2023_date %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------TYPE OF BIKE---------------------

#total rides by member type 
rides2023_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
rides2023_date %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------HOUR-------------------------

#average ride_length by member type
rides2023_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
rides2023_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---

#average ride length by member type
rides2023_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
rides2023_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
rides2023_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
rides2023_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
rides2023_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
rides2023_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
rides2023_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------

#average ride length by member type for summer 
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
rides2023_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------

#average ride length by member type
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----

#average ride length by member type
rides2023_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
rides2023_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----

#average ride length by member type
rides2023_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
rides2023_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))