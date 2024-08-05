#' BTC1855 - Midterm, Zachery Chan
#' R version: Version 2024.04.2+764 (2024.04.2+764)
#' Code dated to Jul 31

#` ---------------------------------------------------------------

# Setting libraries 
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(lubridate)
library(corrplot)


#` ---------------------------------------------------------------

# Creating functions

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


#` ---------------------------------------------------------------

# Reading raw data 
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


#` ---------------------------------------------------------------

# Exploratory fucntions for datasets 
glimpse(trip)
summary(trip)
# Notably, massive positive skew for trip duration, majority is much closer to 0 

glimpse(station)
summary(station)

glimpse(weather)
summary(weather)

# Checking for missingness in datasets 
anyNA(trip)
empty_string(trip)
# Need to address empty strings in trip 

anyNA(weather)
empty_string(weather)
# Need to address empty strings in trip 
# NAs are mainly in events and max wind speed

# Saving data in new frames to retain data integrity 
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

# Factoring start station id
trip1$end_station_id_fctr <- factor(trip1$end_station_id)

# Factoring end station id
trip1$start_station_id_fctr <- factor(trip1$start_station_id)

# Factoring start station name
trip1$end_station_name_fctr <- factor(trip1$end_station_name)

# Factoring end station name
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

# Factoring station id to match with trip dataset
station1$id_fctr <- factor(station1$id)

# Converting to POSix
station1$installation_date_alt <- mdy(station1$installation_date)

# Saving corrected structure for trip dataset in new dataframe
trip2 <- trip1 %>% 
  select(-start_date, - end_date, -start_station_name, -subscription_type, 
         -end_station_name, -start_station_id, end_station_id)  %>%
  select(id, duration_seconds = duration, duration_POSix = duration_alt, 
         start_date = start_date_alt, start_station = start_station_name_fctr, 
         start_station_id = start_station_id_fctr, end_date = end_date_alt, 
         end_station = end_station_name_fctr, end_station_id = end_station_id_fctr, 
         bike_id, subscription_type = subscription_type_fctr, zip_code)

# Saving corrected structure for weather dataset in new dataframe
weather2 <- weather1 %>% 
  select(-date, -events, -city) %>% 
  select(date = date_alt, max_temperature_f, mean_temperature_f, min_temperature_f,
         max_visibility_miles, mean_visibility_miles, min_visibility_miles, 
         max_wind_speed_mph = max_wind_Speed_mph, mean_wind_speed_mph, max_gust_speed_mph,
         precipitation_inches, cloud_cover, , events = events_fctr, zip_code, city = city_fctr)

# Saving corrected structure for station dataset in new dataframe
station2 <- station1 %>% 
  select(-installation_date, -id, -city) %>% 
  select(id = id_fctr, name, , lat, long, dock_count, city = city_fctr, installation_date =installation_date_alt)


#` ---------------------------------------------------------------

# Identifying cancelled trips 

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

# The vast majority of trips are of short duration, with extreme values creating large positive skew 
sort((trip3$duration_minutes/(60)), decreasing = T)

describe(trip3)
quantile(trip3$duration_seconds, c(0.25, 0.75))

# Using standard deviation to determine upper limit
std_dev <- sd(trip3$duration_seconds, na.rm = TRUE)

duration_mean <- mean(trip3$duration_seconds, na.rm = TRUE)

# Defining upper limit for duration, using 3*standard deviation as upper limit
# to account for huge positive skew and 3 times equates to approx. 26 hours.
# Just over 1 day is a reasonable upper limit for a BIKE rental 
max_limit <- duration_mean + 3*std_dev

sort(trip3$duration_seconds, decreasing = F)
# As using st dev is not feasible for lower limit (as it becomes negative), 

lower_limit <- (0.5)*quantile(trip3$duration_seconds, 0.25)
# Basing lower limit on the IQR range, using 2 X 25% quartile to set lower limit as 
# a lot of data 
# This sets lower limit 180 seconds (set by personal choice, assuming 180 seconds
# is an adequate amount of time for a "test ride" or really fast ride around a station

# Removing outliers and saving as new dataset
trip4 <- trip3 %>% 
  filter(duration_seconds <= max_limit & duration_seconds >= lower_limit)

# Saving outlier ids
outlier_frame <- trip3 %>% 
  filter(!(duration_seconds <= max_limit & duration_seconds >= lower_limit))

outliers <- outlier_frame$id 

log_duration <- log10(trip4$duration_minutes)

dur_lbls <- c("<3 min", "10 min", " 30 min", " 1 hr",
               " 3 hr", "6 hr", "12 hr", "28 hr")

dur_ticks = c(0.5, 10, 30, 60, 180, 360, 720, 1680)

dur_ticks_log <- log10(dur_ticks)

suppressWarnings(hist(trip4$duration_minutes, main= "Trip Duration in Minutes",
                      xlab = "Log(duration) (mins)", ylab = "Frequency of Trips", 
                      breaks = dur_ticks, freq = T, xaxt = "n",))
axis(side = 1, at = dur_ticks, labels = dur_lbls, las = 2, cex.axis = 0.7, cex.lab = 0.5)


# Conducting EDA (post-processing for report) 
basic_eda(trip4)

dur_lbls <- c("<3 min", "10 min", "30 min", "1 hr", "3 hr", "6 hr", "12 hr", "28 hr")
dur_ticks <- c(0.5, 10, 30, 60, 180, 360, 720, 1680)
dur_ticks_log <- log10(dur_ticks)

ggplot(trip4, aes(x = log_duration)) +
  geom_histogram(bins = length(dur_ticks) - 1, fill = "lightblue", color = "black") +
  scale_x_continuous(labels = dur_lbls, breaks = dur_ticks_log) +
  labs(title = "Trip Duration in Minutes", x = "Duration (mins)", y = "Frequency of Trips") 

#` ----------------------------------------------------------------

# Identifying Rush Hours 

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
ggplot(sundays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Monday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Mutating to get hour and month for Tuesday, sorting by month and filtering 
tuesdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 3) %>%
  group_by(month) 

# Plotting histogram for start times on Tuesdays of every month 
ggplot(tuesdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#3399FF", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Tuesday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Mutating to get hour and month for Wednesday, sorting by month and filtering 
wednesdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 4) %>%
  group_by(month) 

# Plotting histogram for start times on Wednesday of every month 
ggplot(wednesdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#FF0000", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Wednesday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Mutating to get hour and month for Thursday, sorting by month and filtering 
thursdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 5) %>%
  group_by(month) 

# Plotting histogram for start times on Thursdays of every month 
ggplot(thursdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#FF3399CC", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Thursday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Mutating to get hour and month for Fridays, sorting by month and filtering 
fridays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 6) %>%
  group_by(month) 

# Plotting histogram for start times on Fridays of every month 
ggplot(fridays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#009933", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Friday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


# Examining the histograms, it seems that the rush hours for weekdays are 
# 06:00 - 09:00 and 15:00 - 18:00. 


#` ---------------------------------------------------------------

# Examining Rush Hours 

# Do rush hours extend to the weekend? 
# Mutating to get hour and month for Saturdays, sorting by month and filtering 
saturdays <- weekends %>%
  mutate(month = month(weekends$start_date)) %>%
  mutate(hour = hour(weekends$start_date)) %>%
  filter(wkday_start == 7) %>%
  group_by(month) 

# Plotting histogram for start times on Saturdays of every month 
ggplot(saturdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#CC66FF", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Saturday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Mutating to get hour and month for Sundays, sorting by month and filtering 
sundays <- weekends %>%
  mutate(month = month(weekends$start_date)) %>%
  mutate(hour = hour(weekends$start_date)) %>%
  filter(wkday_start == 1) %>%
  group_by(month) 

# Plotting histogram for start times on Sundays of every month 
ggplot(sundays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#3399CC", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Sunday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# No rush hours on weekends, only for weekdays 

# Creating variable to list hour of start time for trip
weekdays$hours <-  hour(weekdays$start_date)

# Rush hours -> 06:00 - 09:00 & 15:00 - 18:00 
# Using dplyr to filter trips in weekdays to just have trips during rush hours
rush_hours <- weekdays %>%
  filter(hours >= 6 & hours <= 18 & !hours %in% c(10, 11, 12, 13, 14)) 
# Using logicals to filter out hours pre-6 and post-18 and NOT 10-14

# Using dplyr to use rush_hours dataset to count the start station names for each trip
# and then arrange them by descending order, all stored as new table. 
rush_start_table <- rush_hours %>%
  count(rush_hours$start_station) %>%
  arrange(desc(n))

# Displaying table for start stations during rush hour 
rush_start_table

# Using dplyr to use rush_hours dataset to count the end station names for each trip
# and then arrange them by descending order, all stored as new table.
rush_end_table <- rush_hours %>%
  count(rush_hours$end_station) %>%
  arrange(desc(n))

# Displaying table for end stations during rush hour 
rush_end_table


#` ---------------------------------------------------------------

# Weekend analysis 

# Using weekends dataframe created previously 

# Using dplyr to use weekends dataset to count the start station names for each trip
# and then arrange them by descending order, all stored as new table. 
wknd_start_table <- weekends %>%
  count(weekends$start_station) %>%
  arrange(desc(n))

# Displaying table for start stations during weekends
wknd_start_table

# Using dplyr to use weekends dataset to count the end station names for each trip
# and then arrange them by descending order, all stored as new table. 
wknd_end_table <- weekends %>%
  count(weekends$end_station) %>%
  arrange(desc(n))

# Displaying table for end stations during weekends
wknd_end_table


#` ---------------------------------------------------------------

# Finding average utilisation of bikes per month

# Similar to previous task, creating month variable and variable for days in month
month_total <- trip4 %>%
  mutate(month = floor_date(start_date, "month")) %>% # Creating new variable for month, dropping days + hours/min
  mutate(month_days = days_in_month(start_date)) %>% # Creating new variable for days in month from start date (in case of leap year)
  group_by(month, month_days) %>%
  summarise(month_duration = sum(duration_seconds, na.rm = T), .groups = 'drop') #Using groups = 'drops' to ensure days in momnth appears in table
# NOTE: using the raw duration in seconds here as sum cannot be used on POSix
# and that using the start and end dates (in POSix) only record the hour & minute 
# of the trip. Using the difference between start and end date (i.e. with 
# time_length() results in rounding up/down to the nearest minute)

# Total time in month is total time available for bikes, so need to find number of bikes
bikes <- n_distinct(trip4$bike_id)

# Calculating average utilisation rate and storing as a variable 
month_total$average <- month_total$month_duration/(bikes*60*60*24*month_total$month_days)*100
# For calculation, calculating number of seconds in month * number of bikes 

# Displaying month's usage and month's average utilisation
month_total

# Creating vector for labels in custom axis for plot of average utilisation per month
month_lbls <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Creating vector for ticks in custom axis for plot of average utilisation per month
month_ticks <- seq(from = 1, to = 12, by = 1)

# Plotting line graph of average utilisation per month
ggplot(month_total, aes(x = month, y = average)) +
  geom_line(color = "blue") +  
  geom_point(color = "red") +  
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Average Bike Utilisation per Month, 2014", x = "Month", y = "Average Bike Utilisation Rate (%)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#` ---------------------------------------------------------------

# Creating Correlation Table For Weather 

# Joining station to trip4
# Need to rename station id in trip 4 to match column name to join datasets 
station2 <- station2 %>%
  rename(start_station_id = id)

# Need to factor id in weather to join (as factored in trip4)
station2$start_station_id <- factor(station2$start_station_id)

# Left join, trip <- station, matching rows on station id
trip5 <- left_join(trip4, station2, by = "start_station_id")

# Removing hours and minutes from POSix in start date to match the dates 
# in weather storing as new variable, using start date to match individual trips
# with correct weather readings
trip5$date <- date(trip5$start_date)

# Left join, trip5 <- weather, matching rows on city + date 
trip6 <- left_join(trip5, weather2, by = c("city", "date"))

# Removing variables to better construct correlation table 
# Removing variables created to match datasets, and non-weather related variables
# like dock count 
trip7 <- trip6 %>% 
  select(-zip_code.x, -name, -city, -installation_date, -bike_id, 
         -lat, -long, -dock_count, -date, -duration_seconds, 
         -start_station, -end_station, -duration_minutes, -zip_code.y) 
# NOTE: Joining caused repeat of zipcode, one as number (char), 
# one as character (int), removed both 

# As correlation requires numbers, need to convert all variables to numeric 

# In precipitation, T = Trace amounts of rain. As smallest measurement of rain 
# is 0.01, setting T = 0.0001 to represent trace amounts 
trip7$precipitation_inches[trip7$precipitation_inches== "T"] <- 0.0001
# NOTE: Did not convert precipitation into numeric earlier, as did not 
# previously conduct analysis using the data and would rather keep "trace amounts" as T 
# rather than defining trace amounts as an arbitrary value. 

# Redefining events factor to keep track of events when converted to numeric 
#' events <- 1 = Fog, 2 = Fog-Rain, 3 = Rain, 4 = Rain-Thunderstorm
trip7$events <- ifelse(trip7$events == "Fog", 1,
       ifelse(trip7$events == "Fog-Rain", 2,
              ifelse(trip7$events == "Rain", 3, 4)))

# Redefining subscription factor to keep track when converted to numeric 
#' subscription_type <- 1 = Subscriber, 0 = Customer
trip7$subscription_type <- ifelse(trip7$subscription_type == "Subscriber", 1, 0) 

# Using for loop to convert all variables to numeric
for (i in seq_len(ncol(trip7))) {
  if (!is.numeric(trip7[[i]])) {
    trip7[[i]] <- as.numeric(trip7[[i]])
  }
} # NOTE: Dates become seconds since epoch (January 1, 1970 (UTC))

# Removing other variables in trip dataset (not of interest with correlation to weather)
trip8 <- trip7 %>% 
  select(-id, -start_station_id, -end_station_id, -subscription_type, end_date)

# Examining the NAs in the data to determine if there are any concerns with correlation 
summary(trip8)
# Considering the large number of NAs in max gust speed and very large number of 
# NAs in events, better to use pairwise to maximize data for events

# Creating correlation table for weather variables
weather_corr <- cor(trip8, use = "pairwise.complete.obs")

# Setting names for correlation matrix (better readibiloty and graphing) 
corr_names <- c("Duration", "Start Date", "End Date", "Max Temp (F)", "Mean Temp (F)", "Min Temp (F)", 
               "Max Vis (Miles)", "Mean Vis (Miles)", "Max Wind Speed (mph)", "Mean Wind Speed (mph)", 
               "Precipitation (Inches)", "Cloud Cover", "Weather Events")

# Assign custom labels to the correlation matrix
colnames(weather_corr) <- corr_names
rownames(weather_corr) <- corr_names

# Displaying weather correlation table for trip data
weather_corr

# Graphing correlation for report
corrplot(weather_corr, method = "square", 
         type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.8)
title(main = "Trip Correlation With Weather", line = 2.5)






