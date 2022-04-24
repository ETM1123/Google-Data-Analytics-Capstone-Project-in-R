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

# Sanity check: datatype, range constraint, consistency 
# Observe data 
glimpse(dataset)

# Check for missing values 
apply(X = is.na(dataset), MARGIN = 2, sum) # takes over 1 min optimize later

## Note: no missing values as expected

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
#Idenify total number of observations from each table
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

# At some point delete str_attr from memory

###############################################################################################
# Observations

## As expected no missing values. 
## Star date the bike rips range: 2020 -2022
## Note our data should range from: 2020 - 2021
## The date collected for DEC 2021  contains 172590 entires and 661 are from  2022 (remove)
## All of the ride_id are unique i.e no duplicative values 
## All attributes have the correct datatype
## Character attributes contain valid entires i.e no empty strings 

# Next: 
## Filter dataset to only include attributes for analysis
## Add extra attributes such as: year, month, day_of_week
## Rename few columns for clarity
## Identify the spread distributions of each attribute
## Focus on membership_statis
################################################################################################

# Copy dataset for backup
modified_dataset <- dataset %>% 
  select(-contains(c("id", "lat", "lng", "table")), 
          membership_status = member_casual,
          ride_length = duration,
          rideable_type) %>% 
  mutate(day_of_week = weekdays(started_at),
         month = format(started_at, format="%m"),
         year =  format(started_at, format="%Y")) %>% 
  filter(year == "2020" | year == "2021")


# Check distribuation of membership_status: level entire dataset
modified_dataset %>% 
  select(membership_status) %>% 
  group_by(membership_status) %>% 
  summarise(frequecny = n()) %>% 
  ggplot(aes(x = membership_status, y= frequecny, color = membership_status, fill = membership_status)) +
  geom_bar(position="dodge", stat="identity")
  
# Check distribuation of membership_status: level year 
modified_dataset %>% 
  select(membership_status, year) %>% 
  group_by(membership_status, year) %>% 
  summarise(frequecny = n()) %>% 
  ggplot(aes(x = membership_status, y= frequecny, fill =year)) +
  geom_bar(position="dodge", stat="identity")

# Check distribuation of membership_status: level month ~ causal
modified_dataset %>% 
  select(membership_status, month, year) %>%
  group_by(membership_status, month, year) %>% 
  summarise(frequecny = n()) %>% 
  ggplot(aes(x = month, y= frequecny, fill =membership_status)) +
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~year) +
  scale_x_discrete(labels=month.abb)

# Percent increase - month over month ~ 2020
monthly_trips_2020 <- modified_dataset %>% 
  select(month, year, membership_status) %>% 
  filter(year == "2020") %>% 
  group_by(month, membership_status) %>% 
  summarise(monthly_trips = n()) %>% 
  as.data.frame()
monthly_trips_2020

# Graph change over month
monthly_trips_2020 %>% 
  group_by(membership_status) %>% 
  mutate(percent_increase = round(
    ((monthly_trips - lag(monthly_trips))/lag(monthly_trips))*100, 2)) %>% 
  ggplot(aes(x = month, y= percent_increase, fill =membership_status)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(labels=month.abb) + 
  ggtitle("2020 Month Over Month: Monthly Bike Trips Percent Change")

## fix the y-ticks (later)

# Percent increase - month over month ~ 2021
monthly_trips_2021 <- modified_dataset %>% 
  select(month, year, membership_status) %>% 
  filter(year == "2021") %>% 
  group_by(month, membership_status) %>% 
  summarise(monthly_trips = n())
monthly_trips_2021

# Graph change over month
monthly_trips_2021 %>% 
  group_by(membership_status) %>% 
  mutate(percent_increase = round(
    ((monthly_trips - lag(monthly_trips))/lag(monthly_trips))*100, 2)) %>% 
  ggplot(aes(x = month, y= percent_increase, fill =membership_status)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(labels=month.abb) +
  ggtitle("2021 Month Over Month: Monthly Bike Trips Percent Change")


###############################################################################################
# Observations

## For 2020 - no bike data up until Apri
## Overall - there are more (be specific later) member riders than causal
## Large (be specific) increase in causal riders from 2020 to 2021
## Frequency of riders (for both usertype: membership holders and causal)
## signifcantly increase between March - August 
## Huge percent increase of monthly riders in Mar 2021 for both users
### causal --> 250% increase, membership -- > 650% increas (need to investigate)
## clear display of seasonality effects 
 
# Next:
# Continue investigation membership status
# Look into ride_length and rideable_type
# Identify seasonallity effects
################################################################################################

# Calculate the mean, min, max of ride_length 
modified_dataset %>% 
  select(ride_length) %>% 
  summarise(average_ride_length = mean(ride_length),
            min_ride_length = min(ride_length),
            max_ride_length = max(ride_length))


# Check distribuation of rideable_type + membership_status (may go in the begining)
ggplot(modified_dataset, aes(x = rideable_type, fill = rideable_type)) +
  geom_bar() +
  facet_wrap(~membership_status)


# Calculate the mode day of the week i.e 
frequency_of_rides_per_day <- modified_dataset %>% 
  select(day_of_week, year) %>% 
  group_by(day_of_week, year) %>% 
  summarise(frequnecy_of_rides = n()) %>% 
  as.data.frame()  
frequency_of_rides_per_day

# Stacked bargraph
## order day of week
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
frequency_of_rides_per_day$day_of_week <- factor(frequency_of_rides_per_day$day_of_week, levels = days)
ggplot(frequency_of_rides_per_day, aes(fill=year, y=frequnecy_of_rides, x=day_of_week)) + 
  geom_bar(position="dodge", stat="identity")
 

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
time_interval <- modified_dataset %>% select(started_at, membership_status, ride_length) %>% 
  mutate(hour_in_miltary_time = as.numeric(format(started_at, format="%H"))) %>% 
  group_by(hour_in_miltary_time, membership_status) %>% 
  summarise(count = n())

# barchart side by stack - hour 
ggplot(time_interval, aes(fill=membership_status, y=count, x=hour_in_miltary_time)) + 
  geom_bar(position="stack", stat="identity")

# barchart side by side - hour 
ggplot(time_interval, aes(fill=membership_status, y=count, x=hour_in_miltary_time)) + 
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
