#' BTC1855 - Midterm, Zachery Chan
#' R version: Version 2024.04.2+764 (2024.04.2+764)

#` ---------------------------------------------------------------

library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(lubridate)

# Function for checking if any empty values (i.e. just "", and not as an NA)
empty_string <- function(col) {
  any(col == "")
}

# Exploratory Descriptive Analysis from Datasciencehereos (listed in assignment)
basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

# Setting file path to read files 
file_path <- c("/Users/zachery/BTC1855-Midterm")

# Reading station data 
station <- read.csv("/Users/zachery/BTC1855-Midterm/data/station.csv", header= T, sep = ",")

# Reading trip data 
trip <- read.csv("/Users/zachery/BTC1855-Midterm/data/trip.csv", header= T, sep = ",")
# while default is comma de-limited and with header rows, good to add to remind the structure of raw data 

# Reading weather data 
weather <- read.csv("/Users/zachery/BTC1855-Midterm/data/weather.csv", header= T, sep = ",")
# while default is comma de-limited and with header rows, good to add to remind the structure of raw data 

# Exploring data 
glimpse(trip)

glimpse(station)

glimpse(weather)

basic_eda(trip)

basic_eda(weather)

# Checking for missingness 
anyNA(trip)
empty_string(trip)

anyNA(weather)
empty_string(weather)

# Saving data in new frame to retain data integrity 
trip1 <- trip

weather1 <- weather 

station1 <- station

# Checking for any "" in any of the columns in trip
for (col_name in colnames(trip1)) {
  if (any(trip1[[col_name]] == "", na.rm = T)){
    trip1[[col_name]][trip1[[col_name]] == ""] <- NA
  } # if any "" are found, rewrote as NA
} 

# Checking for any "" in any of the columns in trip
for (col_name in colnames(weather1)) {
  if (any(weather1[[col_name]] == "", na.rm = T)){
    weather1[[col_name]][weather1[[col_name]] == ""] <- NA
  } # if any "" are found, rewrote as NA 
} 

# Cleaning trip data 
# Factoring station information 
# Factoring station id (start & end) 
trip1$end_station_id_fctr <- factor(trip1$end_station_id)

trip1$start_station_id_fctr <- factor(trip1$start_station_id)

# Factoring station name (start & end) 
trip1$end_station_name_fctr <- factor(trip1$end_station_name)

trip1$start_station_name_fctr <- factor(trip1$start_station_name)

# Factoring subscription type 
trip1$subscription_type_fctr <- factor(trip1$subscription_type)

# Converting dates into POSix 
# Converting start date of trip
trip1$start_date_alt <- mdy_hm(trip1$start_date)

# Converting end date of trip
trip1$end_date_alt <- mdy_hm(trip1$end_date)

# Converting duration of trip into POSix
trip1$duration_alt <- as.POSIXct(trip1$duration)


# Cleaning weather data 
#NOTE: T in precipitation is TRACE AMOUNTS of preciptation 

# Factoring rain
# 1 instance of rain was listed as "rain" instead of "Rain", binning "rain" into "Rain"
weather1$events[weather1$events == "rain"] <- "Rain"

weather1$events_fctr <- factor(weather1$events)

# Converting date into POSix
weather1$date_alt <- mdy(weather1$date)

# Factoring city
weather1$city_fctr <- factor(weather1$city)

# Cleaning station data 
# Overall station data is very clean, only need to factor city (only a few cities)
# and convert dates into POSix

# Factoring city
station1$city_fctr <- factor(station1$city)

# Converting to POSix
station1$installation_date_alt <- mdy(station1$installation_date)

# Saving new data structures in new data frames
trip2 <- data.frame(trip1$id, trip1$duration, trip1$duration_alt, trip1$start_date_alt, 
                    trip1$start_station_name_fctr, trip1$start_station_id_fctr, 
                    trip1$end_date_alt, trip1$end_station_name_fctr, 
                    trip1$end_station_id_fctr, trip1$bike_id, 
                    trip1$subscription_type_fctr, trip1$zip_code)
colnames(trip2) <- c("id", "duration_seconds", "duration_POSix", "start_date",
                     "start_station", "start_station_id", "end_date", 
                     "end_station", "end_station_id", "bike_id", "subscription_type", 
                     "zip_code")

weather2 <- data.frame(weather1$date_alt, weather1$max_temperature_f, 
                       weather1$mean_temperature_f, weather1$min_temperature_f, 
                       weather1$max_visibility_miles, weather1$mean_visibility_miles, 
                       weather1$min_visibility_miles, weather1$max_wind_Speed_mph, 
                       weather1$mean_wind_speed_mph, weather1$max_gust_speed_mph, 
                       weather1$precipitation_inches, weather1$cloud_cover, 
                       weather1$events_fctr, weather1$zip_code,
                       weather1$city_fctr)

colnames(weather2) <- c(
  "date", "max_temperature_f", "mean_temperature_f", "min_temperature_f", 
  "max_visibility_miles", "mean_visibility_miles", "min_visibility_miles", 
  "max_wind_speed_mph", "mean_wind_speed_mph", "max_gust_speed_mph", 
  "precipitation_inches", "cloud_cover", "events", "zip_code", "city")

station2 <- data.frame(station1$id, station1$name, station1$lat, station1$long, 
                       station1$dock_count, station1$city_fctr, station1$installation_date_alt)

colnames(station2) <- c("id", "name", "lat", "long", "dock_count", "city",
                        "installation_date")

#` ---------------------------------------------------------------

# Identifying cancelled trips of trips with less than 180 seconds (3 minutes)
# and trips that start and end at the same station, storing trip ids 
cancelled <- trip2 %>% 
  filter(duration_seconds < 180 & start_station_id == end_station_id)

# Storing cancelled trips 
cancelled_trips <- cancelled$id

# Removing cancelled trips from trip dataset
trip3 <- trip2 %>% 
  filter(!(duration_seconds < 180 & start_station_id == end_station_id))


#` ---------------------------------------------------------------

# Identifying outliers 

# Converting duration into minutes to more easily comprehend
trip3$duration_minutes <- trip3$duration_seconds/60

summary(trip3)
# As the other variables are descriptive (i.e. describing the context of the trip)
# the only major concern for outliers is the duration variable (as it will correlate
# with other outliers in start/end date, etc.)

# From EDA earlier (histogram of duration), the mast majority of trips are 
# of short duration, with extreme values creating large positive skew 
sort(trip3$duration_minutes, decreasing = T)

# Using standard deviation to determine upper limit
std_dev <- sd(trip3$duration_seconds, na.rm = TRUE)

duration_mean <- mean(trip3$duration_seconds, na.rm = TRUE)

# Defining upper limit for duration, using 3.5*standard deviation as upper limit
# to account for huge positive skew and 3.5 times equates to approx. 75 hours.
# Just over 2 days is a reasonable upper limit for a BIKE rental 
max_limit <- duration_mean + 3.5*std_dev

# Saving outlier ids
otler <- trip3 %>% 
  filter(!(duration_seconds <= max_limit & duration_seconds >= 180))

outliers <- otler$id 

sort(trip3$duration_minutes, decreasing = F)
# As using st dev is not feasible for lower limit (as it becomes negative), 
# using lower limit of 180 seconds (set by personal choice, assuming 180 seconds
# is an adequate amount of time for a "test ride" or really fast ride around a station

# Removing outliers and saving as new dataset
trip4 <- trip3 %>% 
  filter(duration_seconds <= max_limit & duration_seconds >= 180)


#` ---------------------------------------------------------------

#Identifying Rush Hours 

# Examining start date as you needa bike when you start a trip, not when you end a trip
summary(trip3$start_date)

# Using wkday() to determine what day of the week the trip started on, 1 = SUNDAY 
trip3$wkday_start <- wday(trip3$start_date)

# Saving weekends as different dataset
weekends <- trip3 %>%
  filter(wkday_start == 1 | wkday_start == 7)

# Saving weekdays as different dataset
weekdays <- trip3 %>%
  filter(!(wkday_start == 1 | wkday_start == 7))

# Mutating weekdays to create month and hour for start date, grouping by month 
# and then sorting to get Mondays for each month
mondays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 2) %>%
  group_by(month) 

# Setting vector to use for graph labels in histograms below
graph_lbls <- seq(from = 0, to = 23, by = 1)

# Plotting histogram for start times on Mondays of every month 
hist(mondays$hour, breaks = 24, main = "Monday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "lightgreen", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

# Mutating to get hour and month for Tuesday, sorting by month and filtering 
tuesdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 3) %>%
  group_by(month) 

# Plotting histogram for start times on Tuesdays of every month 
hist(tuesdays$hour, breaks = 24, main = "Tuesday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "lightblue", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

# Mutating to get hour and month for Wednesday, sorting by month and filtering 
wednesdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 4) %>%
  group_by(month) 

# Plotting histogram for start times on Wednesday of every month 
hist(tuesdays$hour, breaks = 24, main = "Wednesday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "#FF66CC", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

# Mutating to get hour and month for Thursday, sorting by month and filtering 
thursdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 5) %>%
  group_by(month) 

# Plotting histogram for start times on Thursdays of every month 
hist(thursdays$hour, breaks = 24, main = "Thursday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "#66CC66", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

# Mutating to get hour and month for Fridays, sorting by month and filtering 
fridays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 6) %>%
  group_by(month) 

# Plotting histogram for start times on Fridays of every month 
hist(fridays$hour, breaks = 24, main = "Friday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "#CC66FF", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

# Examining the histograms, it seems that the rush hours for weekdays are 
# 06:00 - 09:00 and 15:00 - 18:00. 


#` ---------------------------------------------------------------

# Do rush hours extend to the weekend? 
# Mutating to get hour and month for Saturdays, sorting by month and filtering 
saturdays <- weekends %>%
  mutate(month = month(weekends$start_date)) %>%
  mutate(hour = hour(weekends$start_date)) %>%
  filter(wkday_start == 7) %>%
  group_by(month) 

# Plotting histogram for start times on Saturdays of every month 
hist(saturdays$hour, breaks = 24, main = "Saturday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "#CC66FF", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

# Mutating to get hour and month for Sundays, sorting by month and filtering 
sundays <- weekends %>%
  mutate(month = month(weekends$start_date)) %>%
  mutate(hour = hour(weekends$start_date)) %>%
  filter(wkday_start == 1) %>%
  group_by(month) 

# Plotting histogram for start times on Sundays of every month 
hist(sundays$hour, breaks = 24, main = "Sunday Trip Start Times, 2014", 
     xlab = "Hour of the Day", col = "#CC66FF", xaxt = "n")
axis(side = 1, at = graph_lbls, labels = graph_lbls, las = 2, cex.axis = 0.7)

#No rush hours on weekends, only for weekdays 

# Creating variable to list hour of start time for trip
weekdays$hours <-  hour(weekdays$start_date)

# Rush hours -> 06:00 - 09:00 & 15:00 - 18:00 
# Using dplyr to filter trips in weekdays to just have trips during rush hours
rush_hours <- weekdays %>%
  filter(hours >= 6) %>% # Filtering pre-06:00 
  filter(hours <= 18) %>% # Filtering post-18:00
  filter(hours != 10) %>% # Filtering out 10:00 - 14:00 
  filter(hours != 11) %>% 
  filter(hours != 12) %>% 
  filter(hours != 13) %>% 
  filter(hours != 14) 

# Using dplyr to use rush_hours dataset to count the start station names for each trip
# and then arrange them by descending order, all stored as new table. 
rush_start_table <- rush_hours %>%
  count(rush_hours$start_station) %>%
  arrange(desc(n))

rush_start_table

# Using dplyr to use rush_hours dataset to count the end station names for each trip
# and then arrange them by descending order, all stored as new table.
rush_end_table <- rush_hours %>%
  count(rush_hours$end_station) %>%
  arrange(desc(n))

rush_end_table


#` ---------------------------------------------------------------





