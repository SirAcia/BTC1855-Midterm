#' BTC1855 - Midterm, Zachery Chan
#' R version: Version 2024.04.2+764 (2024.04.2+764)
#' Github Repo: https://github.com/SirAcia/BTC1855-Midterm.git
#' Code dated to Aug 6

#` ---------------------------------------------------------------

#########################################################
################ Setting Libraries ######################
#########################################################

library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(lubridate)
library(corrplot)
library(cowplot)


#` ---------------------------------------------------------------

#########################################################
######### Defining Custom Functions #####################
#########################################################

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

#########################################################
#################  Reading Raw Data #####################
#########################################################

# Setting file path to read files 
file_path <- c("/Users/zachery/BTC1855-Midterm")

# Reading station data 
station <- read.csv("/Users/zachery/BTC1855-Midterm/Raw data/station.csv", header= T, sep = ",")

# Reading trip data 
trip <- read.csv("/Users/zachery/BTC1855-Midterm/Raw data/trip.csv", header= T, sep = ",")
# while default is comma de-limited and with header rows, good to add to remind the structure of raw data 

# Reading weather data 
weather <- read.csv("/Users/zachery/BTC1855-Midterm/Raw data/weather.csv", header= T, sep = ",")
# while default is comma de-limited and with header rows, good to add to remind the structure of raw data 


#` ---------------------------------------------------------------

#########################################################
######### Exploratory Functions & Cleaning ##############
#########################################################

# Exploratory functions for datasets 
glimpse(trip)
summary(trip)
# Notably, massive positive skew for trip duration, majority is much closer to 0 

glimpse(station)
summary(station)

glimpse(weather)
summary(weather)
# Lots of missingness for precipitation and weather events

# Checking for missingness in datasets 
anyNA(trip)
empty_string(trip)
# Need to address empty strings in trip 

anyNA(weather)
empty_string(weather)
# Need to address empty strings in trip 
# NAs/"" are mainly in events, precipitation, max gust speed 

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

# Converting dates into POSIX
# Converting start date of trip
trip1$start_date_alt <- mdy_hm(trip1$start_date)

# Converting end date of trip
trip1$end_date_alt <- mdy_hm(trip1$end_date)

# Converting duration of trip into POSIX
trip1$duration_alt <- as.POSIXct(trip1$duration)


# Cleaning weather data 
#NOTE: T in precipitation is TRACE AMOUNTS of preciptation 

# Factoring rain
# 1 instance of rain was listed as "rain" instead of "Rain", binning "rain" into "Rain"
weather1$events[weather1$events == "rain"] <- "Rain"

weather1$events_fctr <- factor(weather1$events)

# Converting date into POSIX
weather1$date_alt <- mdy(weather1$date)

# Factoring city
weather1$city_fctr <- factor(weather1$city)

# Cleaning station data 
# Overall station data is very clean, only need to factor city (only a few cities)
# and convert dates into POSIX

# Factoring city
station1$city_fctr <- factor(station1$city)

# Factoring station id to match with trip dataset
station1$id_fctr <- factor(station1$id)

# Factoring station name to match with trip dataset
station1$name_fctr <- factor(station1$name)

# Converting to POSIX
station1$installation_date_alt <- mdy(station1$installation_date)

# Saving corrected structure for trip dataset in new dataframe
trip2 <- trip1 %>% 
  select(-start_date, - end_date, -start_station_name, -subscription_type, 
         -end_station_name, -start_station_id, end_station_id)  %>%
  select(id, duration_seconds = duration, duration_POSIX = duration_alt, 
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
# Fixing typo in "max wind speed" column 

# Saving corrected structure for station dataset in new dataframe
station2 <- station1 %>% 
  select(-installation_date, -id, -city) %>% 
  select(id = id_fctr, name = name_fctr, , lat, long, dock_count, city = city_fctr, installation_date =installation_date_alt)

#Creating weather dataframe with different labels for EDA graphs 
weather_graph <-  weather2 %>% 
  select("Date" = date,  "Max Temp (F)" = max_temperature_f, "Mean Temp (F)" = mean_temperature_f, 
         "Min Temp (F)" = min_temperature_f, "Max Visibility (miles)" = max_visibility_miles, 
         "Mean Visibility (miles)" = mean_visibility_miles, "Min Visibility (miles)" = min_visibility_miles, 
         "Max Wind Speed (mph)" = max_wind_speed_mph, "Mean Wind Speed (mph)" = mean_wind_speed_mph, 
         "Max Gust Speed (mph)" = max_gust_speed_mph, "Precipitation (inches)" = precipitation_inches, 
         "Cloud Cover" = cloud_cover, "Weather Events" = events, "Zip Code" = zip_code, "City" = city)

# Basic EDA for weather data 
basic_eda(weather_graph)

#` ---------------------------------------------------------------

#########################################################
########### Identifying Cancelled Trips #################
#########################################################

# Identifying cancelled trips of trips with less than 180 seconds (3 minutes)
# and trips that start and end at the same station, storing trip ids 
cancelled_trips <- trip2 %>% 
  filter(duration_seconds < 180 & start_station_id == end_station_id)

# Saving cancelled trips as .csv
write.csv(cancelled_trips, file = "BTC_1855_Midterm_Cancelled_Trips.csv", row.names = FALSE)


# Removing cancelled trips from trip dataset
trip3 <- trip2 %>% 
  filter(!(duration_seconds < 180 & start_station_id == end_station_id))


#` ---------------------------------------------------------------

#########################################################
############## Identifying Outliers #####################
#########################################################

# Converting duration into minutes to more easily comprehend
trip3$duration_minutes <- trip3$duration_seconds/60

summary(trip3)
# As the other variables are descriptive (i.e. describing the context of the trip)
# the only major concern for outliers is the duration variable (as it will correlate
# with other outliers in start/end date, etc.)

# The vast majority of trips are of short duration, with extreme values creating large positive skew 
sort((trip3$duration_minutes/(60)), decreasing = T)
# Sorting duration IN HOURS by decreasing order
# Seems to be obvious outliers of trips with lasting 4000+ hours, 2000+ hours,etc. 

describe(trip3)
quantile(trip3$duration_seconds, c(0.25, 0.75))

# Using standard deviation to determine upper limit
std_dev <- sd(trip3$duration_seconds, na.rm = TRUE)

duration_mean <- mean(trip3$duration_seconds, na.rm = TRUE)

# Defining upper limit for duration, using 3*standard deviation as upper limit
# to account for huge positive skew and 3 times equates to approx. 26 hours.
# Just over 1 day is a reasonable upper limit for a BIKE rental 
max_limit <- duration_mean + 3*std_dev

duration_mean - std_dev
# Using st dev is not feasible for lower limit (as it becomes negative), 

sort(trip3$duration_seconds, decreasing = F)
# Sorting duration IN SECONDS by increasing order
# If not a cancelled trip, trips of approx. 1 minute between stations seems extreme

lower_limit <- (0.5)*quantile(trip3$duration_seconds, 0.25)
# Basing lower limit on the IQR range, using 2 X 25% quartile to set lower limit as 
# a lot of data 
# This sets lower limit 180 seconds (set by personal choice), assuming 180 seconds
# is an adequate amount of time for a "test ride" or really fast ride around a station

# Removing outliers and saving as new dataset
trip4 <- trip3 %>% 
  filter(duration_seconds <= max_limit & duration_seconds >= lower_limit)

# Saving data frame of just outliers 
outliers <- trip3 %>% 
  filter(!(duration_seconds <= max_limit & duration_seconds >= lower_limit)) %>%
  select(-duration_minutes)
# Removing wkday start and duration minutes as those are calculated variables for my analysis, 
# and may be more confusing in later analysis as they do not originate with the raw dataset

# Saving outliers as .csv
write.csv(outliers, file = "BTC_1855_Midterm_Outliers.csv", row.names = FALSE)

# Creating new dataframe for trip duration histogram 
duration <- trip4

# using log_10 to more easily visualize the data 
duration$log_duration <- log10(duration$duration_minutes)

# Setting histogram labels, and tick marks
dur_lbls <- c("~0", "1", "2", "5", "10", "30", "1hr", "3hr", "6hr", "12hr", "24+hr")
dur_ticks <- c(0.5, 1, 2, 5, 10, 30, 60, 180, 360, 720, 1680)  # in minutes
dur_ticks_log <- log10(dur_ticks)


# Plot histogram with custom log scale labels
ggplot(duration, aes(x = log_duration)) +
  geom_histogram(binwidth = 0.08, fill = "lightblue", color = "black") +
  labs(title = "Trip Duration in Minutes", x = "Log of Duration (mins)", y = "Frequency of Trips") + 
  scale_x_continuous(breaks = dur_ticks_log, labels = dur_lbls) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Storing cleaned data as new data frame for presentation in report,
# easier to set column names without downstream impacts
cleaned_data <- trip4 %>%
  select(-duration_seconds, -duration_minutes)
# Removing duration minutes & seconds as those measure the same variable and were used for understanding/plotting

# Setting column names for better presentation in EDA functions
colnames(cleaned_data) <- c("ID", "Duration", "Start Date", "Start Station", "Start_Station ID", 
                            "End Date", "End Station", "End Station ID", "Bike ID", "Subscription Type", 
                            "Zip Code ")

# Using EDA functions to get summary graphs of data (beyond trip duration)
basic_eda(cleaned_data)

#` -----------------------------------------------------------------------------

#########################################################
############## Identifying Rush Hours ###################
#########################################################

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
mon_plot <- ggplot(mondays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Monday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

mon_plot

# Mutating to get hour and month for Tuesday, sorting by month and filtering 
tuesdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 3) %>%
  group_by(month) 

# Plotting histogram for start times on Tuesdays of every month 
tue_plot <- ggplot(tuesdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#3399FF", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Tuesday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

tue_plot

# Mutating to get hour and month for Wednesday, sorting by month and filtering 
wednesdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 4) %>%
  group_by(month) 

# Plotting histogram for start times on Wednesday of every month 
wed_plot <- ggplot(wednesdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#FF0000", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Wednesday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

wed_plot

# Mutating to get hour and month for Thursday, sorting by month and filtering 
thursdays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 5) %>%
  group_by(month) 

# Plotting histogram for start times on Thursdays of every month 
thu_plot <- ggplot(thursdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#FF3399CC", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Thursday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

thu_plot

# Mutating to get hour and month for Fridays, sorting by month and filtering 
fridays <- weekdays %>%
  mutate(month = month(weekdays$start_date)) %>%
  mutate(hour = hour(weekdays$start_date)) %>%
  filter(wkday_start == 6) %>%
  group_by(month) 

# Plotting histogram for start times on Fridays of every month 
fri_plot <- ggplot(fridays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#009933", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Friday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

fri_plot

# Examining the histograms, it seems that the rush hours for weekdays are 
# 06:00 - 09:00 and 15:00 - 18:00. 

#` -----------------------------------------------------------------------------

#########################################################
############# Examining Rush Hours  #####################
#########################################################

# Do rush hours extend to the weekend? 
# Mutating to get hour and month for Saturdays, sorting by month and filtering 
saturdays <- weekends %>%
  mutate(month = month(weekends$start_date)) %>%
  mutate(hour = hour(weekends$start_date)) %>%
  filter(wkday_start == 7) %>%
  group_by(month) 

# Plotting histogram for start times on Saturdays of every month 
sat_plot <- ggplot(saturdays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#CC66FF", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Saturday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

sat_plot

# Mutating to get hour and month for Sundays, sorting by month and filtering 
sundays <- weekends %>%
  mutate(month = month(weekends$start_date)) %>%
  mutate(hour = hour(weekends$start_date)) %>%
  filter(wkday_start == 1) %>%
  group_by(month) 

# Plotting histogram for start times on Sundays of every month 
sun_plot <- ggplot(sundays, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "#3399CC", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = graph_lbls, labels = graph_lbls) +
  labs(title = "Sunday Trip Start Times, 2014", x = "Hour of the Day", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

sun_plot

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

# Setting column names for better readability 
colnames(rush_start_table) <- c("start_station", "n")

# Displaying table for start stations during rush hour 
rush_start_table

# Graph of start station use during rush hour 
ggplot(rush_start_table, aes(x = reorder(start_station, n), y = n, fill = start_station)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flipping axis 
  xlab("Start Station") +
  ylab("Count") +
  ggtitle("Start Stations During Rush Hour") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  guides(fill = "none")

# Using dplyr to use rush_hours dataset to count the end station names for each trip
# and then arrange them by descending order, all stored as new table.
rush_end_table <- rush_hours %>%
  count(rush_hours$end_station) %>%
  arrange(desc(n))

# Setting column names for better readability 
colnames(rush_end_table) <- c("end_station", "n")

# Displaying table for end stations during rush hour 
rush_end_table

# Graph of end station use during rush hour 
ggplot(rush_end_table, aes(x = reorder(end_station, n), y = n, fill = end_station)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flipping axis 
  xlab("End Station") +
  ylab("Count") +
  ggtitle("End Stations During Rush Hour") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  guides(fill = "none")


# Combining different weekday plots for report
week_plot <- plot_grid(
  mon_plot, tue_plot, wed_plot, 
  thu_plot, fri_plot, sat_plot, 
  sun_plot, 
  ncol = 3,  # Number of columns
  nrow = 3   # Number of rows
)

week_plot

#` -----------------------------------------------------------------------------

#########################################################
############# Weekend analysis ##########################
#########################################################

# Using weekends dataframe created previously 

# Using dplyr to use weekends dataset to count the start station names for each trip
# and then arrange them by descending order, all stored as new table. 
wknd_start_table <- weekends %>%
  count(weekends$start_station) %>%
  arrange(desc(n))

# Setting column names for better readability 
colnames(wknd_start_table) <- c("start_station", "n")

# Displaying table for start stations during weekends
wknd_start_table

# Creating graph for report 
ggplot(wknd_start_table, aes(x = reorder(start_station, n), y = n, fill = start_station)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flipping axis 
  xlab("Start Station") +
  ylab("Count") +
  ggtitle("Start Stations on Weekends") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  guides(fill = "none")

# Using dplyr to use weekends dataset to count the end station names for each trip
# and then arrange them by descending order, all stored as new table. 
wknd_end_table <- weekends %>%
  count(weekends$end_station) %>%
  arrange(desc(n))

# Setting column names for better readability 
colnames(wknd_end_table) <- c("end_station", "n")

# Displaying table for end stations during weekends
wknd_end_table

# Creating graph for report 
ggplot(wknd_end_table, aes(x = reorder(end_station, n), y = n, fill = end_station)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flipping axis 
  xlab("End Station") +
  ylab("Count") +
  ggtitle("End Stations on Weekends") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  guides(fill = "none")

#` -----------------------------------------------------------------------------

#########################################################
########### Average Utilisation Per Month ###############
#########################################################

# Similar to previous task, creating month variable and variable for days in month
month_total <- trip4 %>%
  mutate(month = floor_date(start_date, "month")) %>% # Creating new variable for month, dropping days + hours/min
  mutate(month_days = days_in_month(start_date)) %>% # Creating new variable for days in month from start date (in case of leap year)
  group_by(month, month_days) %>%
  summarise(month_duration = sum(duration_seconds, na.rm = T), .groups = 'drop') #Using groups = 'drops' to ensure days in momnth appears in table
# NOTE: using the raw duration in seconds here as sum cannot be used on POSIX
# and that using the start and end dates (in POSIX) only record the hour & minute 
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

#` -----------------------------------------------------------------------------

#########################################################
############# Weather Correlation #######################
#########################################################

# Joining station to trip4
# Need to rename station id in trip 4 to match column name to join datasets 
station2 <- station2 %>%
  rename(start_station_id = id)

# Need to factor id in weather to join (as factored in trip4)
station2$start_station_id <- factor(station2$start_station_id)

# Left join, trip <- station, matching rows on station id
trip5 <- left_join(trip4, station2, by = "start_station_id")

# Removing hours and minutes from POSIX in start date to match the dates 
# in weather storing as new variable, using start date to match individual trips
# with correct weather readings
trip5$date <- date(trip5$start_date)

# Grouping by city and date to calculate the number of trips per day in each city 
trips_per_day <- trip5 %>%
  group_by(city, date) %>%
  summarise(trips = n(), .groups = 'drop') # dropping grouping here

# Joining number of trips to trip data, keeps the original number of 
# observations but calculates the number of trips for that day 
trip6 <- left_join(trip5, trips_per_day, by = c("city", "date"))

# Removing unnecessary variables (do not need to look at correlation for factored or variables not of interest)
trip6 <- trip6 %>% 
  select(-duration_seconds, -start_date, -start_station, -start_station_id, 
         -end_date, -end_station, end_station_id, -bike_id, -zip_code, -duration_minutes, 
         -name, -lat, -long, -dock_count, -installation_date, -end_station_id, -subscription_type) 


# Left join, trip5 <- weather, matching rows on city + date 
trip7 <- left_join(trip6, weather2, by = c("city", "date"))

# Removing variables used to match datasets and any other variables 
# to better construct correlation table 
trip7 <- trip7 %>% 
  select(-id, -zip_code, -date, -city)

# As correlation requires numbers, need to convert all variables to numeric 

# In precipitation, T = Trace amounts of rain. As smallest measurement of rain 
# is 0.01, setting T = 0.0001 to represent trace amounts 
trip7$precipitation_inches[trip7$precipitation_inches== "T"] <- 0.0001
# NOTE: Did not convert precipitation into numeric earlier, as did not 
# previously conduct analysis using the data and would rather keep "trace amounts" as T 
# rather than defining trace amounts as an arbitrary value. 

# Redefining events factor to keep track of events when converted to numeric 
#' events <- 1 = Fog, 2 = Fog-Rain, 3 = Rain, 4 = Rain-Thunderstorm. 
#' Ranking weather events in terms of severity
trip7$events <- ifelse(trip7$events == "Fog", 1,
       ifelse(trip7$events == "Fog-Rain", 2,
              ifelse(trip7$events == "Rain", 3, 4)))


# Using for loop to convert all variables to numeric
for (i in seq_len(ncol(trip7))) {
  if (!is.numeric(trip7[[i]])) {
    trip7[[i]] <- as.numeric(trip7[[i]])
  }
} # NOTE: Duration become seconds since epoch (January 1, 1970 (UTC))

# Examining the NAs in the data to determine if there are any concerns with correlation 
summary(trip7)
# Considering the large number of NAs in max gust speed and very large number of 
# NAs in events, better to use pairwise to maximize data for events

# Creating correlation table for weather variables
weather_corr <- cor(trip7, use = "pairwise.complete.obs")

# Setting names for correlation matrix (better readibility and graphing) 
corr_names <- c("Trip Duration", "Number of Trips", "Max Temp F", "Mean Temp F", "Min Temp F", 
               "Max Visibilty (miles)", "Mean Visibility (miles)", "Min Visibility (miles)", 
               "Max Wind Speed (mph)", "Mean Wind Speed (mph)", "Max Gust Speed (mph)",
               "Precipitation (Inches)", "Cloud Cover", "Weather Events")

# Assign custom labels to the correlation matrix
colnames(weather_corr) <- corr_names
rownames(weather_corr) <- corr_names

# Displaying weather correlation table for trip data
weather_corr

# Graphing correlation for report
corrplot(weather_corr, method = "square", tl.col = "black", tl.srt = 45, tl.cex = 0.6)
title(main = "Trip Correlation With Weather", line = 3)







