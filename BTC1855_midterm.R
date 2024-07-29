

library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)

empty_string <- function(col) {
  any(col == "")
}

rmve_blank <- function(data, column_name) {
  data[[paste0(column_name, "_alt")]][data[[paste0(column_name, "_fctr")]] ==""] <- NA 
  #' Redefining levels of factor to remove "" bin as it is empty 
  new_lvls <- levels(data[[paste0(column_name, "_fctr")]])
  new_lvls <- new_lvls[new_lvls != ""] 
  #' Redefining factor with new levels 
  data[[paste0(column_name, "_fctr")]] <- factor(data[[paste0(column_name, "_fctr")]], levels =   new_lvls)
}

for (col_name in colnames(trip)) {
  if (any(col == "") == T){
    
    trip[[col_name]][trip[[col_name]] == ""] <- NA
    
  }
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

#Fixing structural issues
anyNA(trip)
anyNA(weather)

#factoring subscription type 

#finding cancelled trips 
which(trip$start_station_id == trip$end_station_id && (trip$duration/60) < 3)
