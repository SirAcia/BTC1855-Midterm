# BTC1855-Midterm

Plan for midterm: 

Read data 

Clean data 
Identify: missing, NAs, structrual issues (variable types, factors, etc.) 

To identify cancelled trips:
- use which for if start == end && duration < 3
- record rows
- filter by same condition, store as new dataframe 

For outliers 
- use summary functions to identify outliers (summary, head, tbl, etc.) 
- use st dev to identify outliers IF cannot identify by eye
- filter by logicals, store as new dataframe

Rush hour
- use histograms to identify hours with highest volume of trips (using lubridate)
- go weekday by weekday, histogram for each to identify hours each day
- filter data under rush hours only and save as new dataframe 

10 most frequent during rush hours 
- using previosu dataframe, use summary stats to examine which are most frequent 

Filter by dates for weekends 
- see when year started, identify pattern to identify weekends
- filter data by weekends and save as new frame 
- using previosu dataframe, use summary stats to examine which are most frequent

average utilisation/month 
- use groupby for month
- then calculate average for each month

Plan for weather joining: 
 The data science team assumes that weather conditions probably have an impact on the bike rental
patterns, but they are not sure whether they should use temperature, weather events, visibility or other
weather measurements available. Help them decide by creating a new dataset combining trip data with the
weather data. (Note that the weather data is available for each city and date. Join your datasets
accordingly). 

Correlation matrix: 
Create a correlation matrix for the new dataset using the cor() function from the corrplot
package. Flag the highest correlations for the data science team.

Report plan: 
 You team lead expects a Data Analysis Report with all your findings for the next meeting in two weeks.

