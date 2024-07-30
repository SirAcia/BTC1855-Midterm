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





