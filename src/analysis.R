###############################################################################################
# This script attempts analyze 2020-2021 bikeshare data from the chicago region 
# in attempt to answer the following question: 
# How do annual members and casual riders use Cyclistic bikes differently?
################################################################################################

# Load packages used in this analysis 
library(tidyverse)

# Clear current working space 
rm(list = ls())

# Set up working directory 
working_directory <- "/Users/eyobmanhardt/desktop/data_analytics/google_capstone_project_R/data/processed"
setwd(working_directory)

# Load data
dataset <- read_csv("chicago_bike_trip_clean.csv")

# Sanity check: datatype, range constraint, consistency 
# Observe data 
glimpse(dataset)

# Check for missing values 
apply(X = is.na(dataset), MARGIN = 2, sum) # takes over 1 min optimize later

# Date range: 2020 - 2021 (at least start_data)
dataset %>% 
  select(started_at) %>% 
  mutate(year = format(started_at, format = "%Y")) %>% 
  group_by(year) %>% 
  summarise(number_of_observations = n()) %>% 
  as.data.frame()

# Idenitfy which table contains start_date of 2022
dataset %>% 
  select(table_name, started_at) %>% 
  mutate(year = format(started_at, format = "%Y")) %>% 
  filter(year == "2022") %>% 
  group_by(table_name) %>% 
  summarise(number_of_observations = n())

# table_name --> indicates which table data observation came from
# Idenify total number of observations from each table
dataset %>% 
  select(table_name) %>% 
  group_by(table_name) %>% 
  summarise(number_of_observations = n()) %>% 
  as.data.frame()

# check for duplications: ride_id ---> should be unique
dataset %>% 
  select(ride_id) %>% 
  summarise(number_of_duplicts = sum(duplicated(ride_id)))

# Check for inconsistent or invalid string attributes
str_attr <- dataset %>% 
  select(contains(c("name", "id")), rideable_type, member_casual) %>% 
  apply( MARGIN = 2, FUN=str_length)

str_attr[str_attr == 0] 

# Remove str_attr
rm(str_attr)

###############################################################################################
# Observations

## Sanity check passed: missing values, inconsisitent values (for string datatypes), duplicts 
## Sanity check failed: range constraint --> date range: 2020 - 2022 but should be 2020 - 2021
## The date collected for DEC 2021  contains 172590 entires and 661 are from  2022 (need to be remove)

# Next: 
## Filter and remove unnessecailry attributes 
## Add extra attributes such as: year, month, day_of_week
## Rename few columns for clarity
## Identify the spread distributions of each attribute
## Focus on membership_statis
################################################################################################

# Copy dataset for backup and only include attributes of interest
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
modified_dataset <- dataset %>% 
  select(-contains(c("id", "lat", "lng", "table")), 
          membership_status = member_casual,
          ride_length = duration,
          rideable_type) %>% 
  mutate(hour_in_miltary_time = as.numeric(format(started_at, format="%H")),
         day_of_week = factor(weekdays(started_at), levels= days),
         month = format(started_at, format="%m"),
         year =  format(started_at, format="%Y")) %>% 
  filter(year == "2020" | year == "2021")

# Identify overall trend (ussage) of bike trips per month for all users
modified_dataset %>% 
  select(month, year) %>%
  group_by(month, year) %>% 
  summarise(frequency = n()) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = month, y = frequency, group = year, color = year)) +
  geom_line() +
  scale_x_discrete(labels=month.abb) +
  ggtitle("Frequency of Monthly bike for 2020 - 2021") +
  ylab("frequency") +
  xlab("")

# Identify distribuation of membership_status: level entire dataset
ggplot(modified_dataset, aes(x = membership_status, fill = membership_status)) +
  geom_bar() +
  ggtitle("Frequency of Causal and Member bike trips for 2020 - 2021")

# Identify distribuation of membership_status: level year 
ggplot(modified_dataset, aes(x = membership_status, group = year, fill = year)) +
  geom_bar(position = "dodge") +
  ggtitle("Frequency of Causal and Member bike trips per Year")

# Identify distribuation of membership_status: level month ~ causal
ggplot(modified_dataset, aes(x = month, group = membership_status, fill = membership_status)) +
  geom_bar(position="dodge") +
  facet_wrap(~year) +
  scale_x_discrete(labels=month.abb) + 
  ggtitle("Frequency of Causal and Member bike trips per month")

# Identify distribuation of membership_status: level day of the week
ggplot(modified_dataset, aes(x =day_of_week, group = membership_status, fill = membership_status)) +
  geom_bar(position="dodge") +
  facet_wrap(~year) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  ggtitle("Frequency of Causal and Member bike trips per Day")

# Identify distribuation of membership_status: level hour of the day
ggplot(modified_dataset, aes(x =hour_in_miltary_time, group = membership_status, fill = membership_status)) +
  geom_bar(position="stack") +
  ggtitle("Proporation of Membership Status of hourly trips for 2020 - 2021")

ggplot(modified_dataset, aes(x =hour_in_miltary_time, group = membership_status, fill = membership_status)) +
  geom_bar(position="dodge") +
  ggtitle("Frequency of Membership Status of hourly trips for 2020 - 2021")

# Calcaulate change over time
monthly_trip <- modified_dataset %>% 
  select(month, year, membership_status) %>% 
  group_by(month, year, membership_status) %>% 
  summarise(monthly_trips = n()) %>% 
  as.data.frame()

# Graph percent increase - month-over-month for 2020
monthly_trip %>% 
  filter(year == "2020") %>% 
  group_by(membership_status) %>% 
  mutate(percent_increase = round(
    ((monthly_trips - lag(monthly_trips))/lag(monthly_trips))*100, 2)) %>% 
  ggplot(aes(x = month, y= percent_increase, fill =membership_status)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(labels=month.abb) + 
  ggtitle("2020 Month Over Month: Monthly Bike Trips Percent Change")

# Graph percent increase - month-over-month for 2021
monthly_trip %>% 
  filter(year == "2021") %>% 
  group_by(membership_status) %>% 
  mutate(percent_increase = round(
    ((monthly_trips - lag(monthly_trips))/lag(monthly_trips))*100, 2)) %>% 
  ggplot(aes(x = month, y= percent_increase, fill =membership_status)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(labels=month.abb) + 
  ggtitle("2021 Month Over Month: Monthly Bike Trips Percent Change")

###############################################################################################
# Observations

## The number of bike trips taken (per month) increases from March - Aug and Decreases of from Aug - Feb
## In 2020 there is no trip data from Jan - Mar 
## 60% of the bike trips are from users with annaul membership status
## From 2020 - 2021 number of bike trips from non-member users increased by 80% 
## From 2020 - 2021 Number of membership holders increased by 50% 
## In Aug 2020, both membership types had the highest monthly bike trips
## In July 2021, both membership types had roughly the sane number of trips
## In 2020, theres a slight increase of daily bike trips from the beggining to the end of the week
## In 2021, there's a significant increase of bike trips during the weekend for casual members
## After midnight, the number of bike trips significantly decreases 
## Both type of members annual, causal, are most active between 10 AM - 10 PM
## In 2021 - the number of casual bike riders increased by over 600% b/w Feb - Mar 

# Next:
# Continue investigation membership status
# Look into ride_length and rideable_type
################################################################################################

# Calculate the mean, min, max of ride_length 
summary(modified_dataset$ride_length)

# Check distribuation of rideable_type + membership_status (may go in the begining)
ggplot(modified_dataset, aes(x = rideable_type, fill = rideable_type)) +
  geom_bar() +
  facet_wrap(~membership_status)

# Calculate the average ride length for the differnt membership types 
modified_dataset %>% 
  select(membership_status, ride_length) %>% 
  group_by(membership_status) %>% 
  summarise(average_ride_length = mean(ride_length), number_of_users = n()) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = membership_status, y = average_ride_length, fill = membership_status)) +
  geom_bar(stat = "identity") + ggtitle("Average Ride Length per Membership status")

# Calculate the average ride_length for users by day_of_week
modified_dataset %>% select(day_of_week, membership_status, ride_length) %>% 
  group_by(membership_status, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length),
            number_of_users = n())

###############################################################################################
# Observations 
## On average, the users (both membership type) ride their bikes for about 15 minutes
## On average, casual members ride their bikes for about 18 minutes
## whereas annual members ride for about 13 minutes
## Casual members roaughly use classic and docked bike at the same rate
## Annual members use classic bikes signifcanlty more than any other bike type

# Next: Identify popular stations and other hot spots or unique stations
################################################################################################