###############################################################################################
# This R script does an ad-hoc analysis on the Lyft's Bikeshare modified_dataset
################################################################################################

# Load packages used in this analysis 
library(tidyverse)

# Set up working directory 
working_directory <- "/Users/eyobmanhardt/desktop/data_analytics/google_capstone_project_R/data/processed"
setwd(working_directory)

# Load data
dataset <- read_csv("chicago_bike_trip_clean.csv")

# Copy dataset for backup
modified_dataset <- dataset %>% 
  select(-contains(c("id", "lat", "lng", "table")), 
          membership_status = member_casual,
          ride_length = duration) %>% 
  mutate(day_of_week = weekdays(started_at),
         month = format(started_at, format="%m"),
         year =  format(started_at, format="%Y")) %>% 
  filter(year == "2020" | year == "2021")



# Calculate the mean, min, max of ride_length
modified_dataset %>% 
  select(ride_length) %>% 
  summarise(average_ride_length = mean(ride_length),
            min_ride_length = min(ride_length),
            max_ride_length = max(ride_length))

# Calculate the mode day of the week i.e 
frequency_of_rides_per_day <- modified_dataset %>% 
  select(day_of_week, year) %>% 
  group_by(day_of_week, year) %>% 
  summarise(frequnecy_of_rides = n()) %>% 
  as.data.frame()  
frequency_of_rides_per_day

# Stacked bargraph
ggplot(frequency_of_rides_per_day, aes(fill=year, y=frequnecy_of_rides, x=day_of_week)) + 
  geom_bar(position="dodge", stat="identity")
 
###############################################################################################
# Observations 
## Average ride length amongest all users is 15 minutes 
## Maximum ride length amongest all users is 48.6 minutes 
## For both years 2020 and 2021 - higher frequency of riders towards the end of the week
## i.e (Friday, Saterday, Sunday)
## Twice as many users in 2021 than 2020 (increase in user might be casue of covid or lack of infromation)
## i.e for 2020 we don't have any data for months Jan - March 
## whereas for 2021 we have data for January - December

# Next: further investigate ussage and membership status 
################################################################################################

# frequency of Users over time (by month)
modified_dataset %>% 
  select(month, year) %>%
  group_by(month, year) %>% 
  summarise(frequency = n()) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = month, y = frequency, group = year, color = year)) +
  geom_line() +
  scale_x_discrete(labels=month.abb) +
  ggtitle("Frequency of bike riders for 2020 - 2021") +
  ylab("frequency") +
  xlab("")

## coninue here
 

# Calculate the average ride length for the differnt membership types 
modified_dataset %>% 
  select(membership_status, ride_length) %>% 
  group_by(membership_status) %>% 
  summarise(average_ride_length = mean(ride_length),
            number_of_users = n())

# Calculate the average ride_length for users by day_of_week
modified_dataset %>% select(day_of_week, membership_status, ride_length) %>% 
  group_by(membership_status, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length),
            number_of_users = n())

# Calculate the frequency of users for each hour in a day
time_interval <- modified_dataset %>% select(started_at, membership_statusl, ride_length) %>% 
  mutate(hour_in_miltary_time = as.numeric(format(started_at, format="%H"))) %>% 
  group_by(hour_in_miltary_time, membership_status) %>% 
  summarise(count = n())

# barchart side by side - hour 
ggplot(time_interval, aes(fill=membership_status, y=count, x=hour_in_miltary_time)) + 
  geom_bar(position="stack", stat="identity")
