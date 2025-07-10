#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

#load CSV files from July 2024 to June 2025
july24_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202407-divvy-tripdata.csv")
aug24_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202408-divvy-tripdata.csv")
sep24_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202409-divvy-tripdata.csv")
oct24_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202410-divvy-tripdata.csv")
nov24_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202411-divvy-tripdata.csv")
dec24_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202412-divvy-tripdata.csv")
jan25_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202501-divvy-tripdata.csv")
feb25_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202502-divvy-tripdata.csv")
mar25_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202503-divvy-tripdata.csv")
apr25_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202504-divvy-tripdata.csv")
may25_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202505-divvy-tripdata.csv")
june25_df <- read_csv("C:/Users/Linh Huynh/Documents/Coursera - Google/Capstone Project/Data - 12 months/months/202506-divvy-tripdata.csv")

#merge all of the data frames into one
cyclistic_df <- rbind (july24_df, aug24_df, sep24_df, oct24_df, nov24_df, dec24_df, jan25_df, feb25_df, mar25_df, apr25_df, may25_df, june25_df)

# Add a "ride_length" calculation for further analysis (in minutes)
cyclistic_df$ride_length <- (as.double(difftime(cyclistic_df$ended_at, cyclistic_df$started_at))) /60

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
str(cyclistic_df)
cyclistic_df$ride_length <- as.numeric(as.character(cyclistic_df$ride_length))

#create columns for: day of week, month, day, year, and hour
cyclistic_df$date <- as.Date(cyclistic_df$started_at) #default format is yyyy-mm-dd, use start date
cyclistic_df$day_of_week <- wday(cyclistic_df$started_at) #calculate the day of the week 
cyclistic_df$day_of_week <- format(as.Date(cyclistic_df$date), "%A") #create column for day of week
cyclistic_df$month <- format(as.Date(cyclistic_df$date), "%m")#create column for month
cyclistic_df$day <- format(as.Date(cyclistic_df$date), "%d") #create column for day
cyclistic_df$year <- format(as.Date(cyclistic_df$date), "%Y") #create column for year
cyclistic_df$time <- format(as.Date(cyclistic_df$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_df$time <- as_hms((cyclistic_df$started_at)) #create new column for time
cyclistic_df$hour <- hour(cyclistic_df$time) #create new column for hour

#create column for different seasons
cyclistic_df <-cyclistic_df %>% mutate(season = 
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

#create column for different time_of_day
cyclistic_df <-cyclistic_df %>% mutate(time_of_day = 
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

#clean data
cyclistic_df <- na.omit(cyclistic_df) #remove rows with NA values
cyclistic_df <- distinct(cyclistic_df) #remove duplicate rows 
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_df <- cyclistic_df %>%  #remove columns not needed
 select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng))
 
#view data
head(cyclistic_df)
View(cyclistic_df)

#export data to CSV
#write.csv(cyclistic_df, "cyclistic_data_cleaned.csv") 

#-------------------------analyze data-------------------------#
#order the day of week and return the average ride length for each day of the week by customer type
cyclistic_df$day_of_week <- ordered(cyclistic_df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual + cyclistic_df$day_of_week, FUN = mean)

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_df$ride_length)
print(cyclistic_avgRide)

#total rides by member type 
cyclistic_df %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
cyclistic_df %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#total number of rides per member type
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(member_casual)

#average ride length by member type and day of week
cyclistic_df %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)		# sorts

#total rides by member type and time of day
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(time_of_day)

#calculate the mean, median, max, and min to observe the data
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = mean)
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = median)
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = max)
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = min)

#number of rides for each time of day
cyclistic_df %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#total rides by member type
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides by member type
cyclistic_df %>%
  count(day_of_week)

#total rides by member type for each month
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62)

#total rides for each month
cyclistic_df %>%
  count(day) %>% 
  print(n = 31)

#average ride length by member type and time of day
cyclistic_df %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for each time of day
cyclistic_df %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length by member type for each month
cyclistic_df %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length for each month
cyclistic_df %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#total rides by member type for each season
cyclistic_df %>%
  group_by(season, member_casual) %>% 
  count(season)

#total rides for each season
cyclistic_df %>%
  group_by(season) %>% 
  count(season)

#average ride length by member type for each season
cyclistic_df %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for each season
cyclistic_df %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))