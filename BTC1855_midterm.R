#' BTC1855 - Midterm, Zachery Chan
#' R version: Version 2024.04.2+764 (2024.04.2+764)
#' Code dated to: July 29

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

