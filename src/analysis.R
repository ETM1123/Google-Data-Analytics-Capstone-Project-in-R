###############################################################################################
# This R script does an ad-hoc analysis on the Lyft's Bikeshare dataset
################################################################################################

# Load packages used in this analysis 
library(tidyverse)

# Set up working directory 
working_directory <- "/Users/eyobmanhardt/desktop/data_analytics/google_capstone_project_R/data/processed"
setwd(working_directory)

# Load data
dataset <- read_csv("chicago_bike_trip_clean.csv")

# Calculate the mean, min, max of duration
dataset %>% select(duration) %>% 
  summarise(average_ride_length = mean(duration),
            min_ride_length = min(duration),
            max_ride_length = max(duration))

# Calculate the mode day of the week 
frequency_of_rides_per_day <- dataset %>% select(started_at) %>% 
  mutate(day_of_week = weekdays(started_at), 
         year =  format(started_at, format="%Y")) %>% 
  group_by(day_of_week, year) %>% 
  summarise(frequnecy_of_rides = n()) %>% 
  as.data.frame() %>% 
  filter(year == "2020" | year == "2021")
frequency_of_rides_per_day

# Stacked bargraph
ggplot(frequency_of_rides_per_day, aes(fill=year, y=frequnecy_of_rides, x=day_of_week)) + 
  geom_bar(position="stack", stat="identity")
 
###############################################################################################
# Observations 
## Average ride length amongest all users is 15 minutes 
## Maximum ride length amongest all users is 48.6 minutes 
## For both years 2020 and 2021 - higher frequency of riders towards the end of the week
## i.e (Friday, Saterday, Sunday)
## Signifcanlty more users in 2020 than 2021 (probably cause by COVID) 

# Next: further investigate ussage and membership status 
################################################################################################

 
# Calculate the average ride length for the differnt membership types 
dataset %>% select(member_casual, duration) %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(duration),
            number_of_users = n())

# Calculate the average ride_length for users by day_of_week
dataset %>% select(started_at, member_casual, duration) %>% 
  mutate(day_of_week = weekdays(started_at)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(duration),
            median_ride_length = median(duration),
            number_of_users = n())

