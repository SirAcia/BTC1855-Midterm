

library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(lubridate)

empty_string <- function(col) {
  any(col == "")
}

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

file_path <- c("/Users/zachery/BTC1855-Midterm")

station <- read.csv("/Users/zachery/BTC1855-Midterm/data/station.csv", header= T, sep = ",")
# whil default is ___, good to add to rmeind the structure of raw data 

trip <- read.csv("/Users/zachery/BTC1855-Midterm/data/trip.csv", header= T, sep = ",")

weather <- read.csv("/Users/zachery/BTC1855-Midterm/data/weather.csv", header= T, sep = ",")

basic_eda(trip)

basic_eda(station)

basic_eda(weather)

anyNA(trip)
empty_string(trip)

anyNA(weather)
empty_string(weather)

trip1 <- trip

weather1 <- weather 

station1 <- station

for (col_name in colnames(trip1)) {
  if (any(trip1[[col_name]] == "", na.rm = T)){
    trip1[[col_name]][trip1[[col_name]] == ""] <- NA
  }
} 

for (col_name in colnames(weather1)) {
  if (any(weather1[[col_name]] == "", na.rm = T)){
    weather1[[col_name]][weather1[[col_name]] == ""] <- NA
  }
} 
glimpse(trip)

#need to factor start station name, end station name, ids, subscription type, make dates into POSIX
# change duragion as well 

trip1$end_station_id_fctr <- factor(trip1$end_station_id)

trip1$end_station_name_fctr <- factor(trip1$end_station_name)

trip1$start_station_id_fctr <- factor(trip1$start_station_id)

trip1$start_station_name_fctr <- factor(trip1$start_station_name)

trip1$subscription_type_fctr <- factor(trip1$subscription_type)

trip1$start_date_alt <- mdy_hm(trip1$start_date)

trip1$end_date_alt <- mdy_hm(trip1$end_date)

trip1$duration_alt <- as.POSIXct(trip1$duration)

glimpse(weather)

#nned tio make date as POSIX, factor city, why T in preceiptation --> TRACE AMOUNTS 

weather1$events[weather1$events == "rain"] <- "Rain"

weather1$events_fctr <- factor(weather1$events)

weather1$date_alt <- mdy(weather1$date)

weather1$city_fctr <- factor(weather1$city)

