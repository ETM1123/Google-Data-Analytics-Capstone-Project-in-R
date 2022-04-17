###############################################################################################
# This R script combines multiple csv files into one 
# and prepares it for analysis - the data consists of 
# Lyft's Bikeshare infromation from the Chicago metropoltian region
################################################################################################

# Load packages used in this analysis 
library(data.table) # Load large data more efficently
library(dplyr) # Data manipulation
library(stringr) # String operations
library(ggplot2) # Plotting
library(testit) # Validation

# Set up working directory 
working_directory <- "/Users/eyobmanhardt/desktop/data_analytics/google_capstone_project_R/data/raw"
setwd(working_directoryr)

# Helper function to track files 
extract_name <- function(str_csv_file){
  # grab the first six charaters of the string
  year_month_string <- substr(str_csv_file, 1,6)
  # extract the year
  year <- substr(year_month_string, 1, 4)
  # extract the month
  month <- substr(year_month_string, 5, 6)
  # combine the year, month and convert to date time
  year_month <- as.Date(paste(year,'-', month, '-', '01', sep = ''))
  # reformat the the datetime object to say the name of the month instead of number
  table_date <- format(year_month, "%b %Y")
  # return (example out 'Dec 2020')
  return(table_date)
}

# Load files
table_files <- list.files(pattern="*.csv")
table_files

# Load data  
list_of_tables <- lapply(table_files, fread)
names(list_of_tables) <- lapply(table_files, extract_name)
chicago_bike_trip <- rbindlist(list_of_tables, idcol = "table_name")

# Convert date fileds into datetime objects 
chicago_bike_trip[, c("started_at", "ended_at"):= .(as.POSIXct(started_at, 
                                                               format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"))]
# Add a new attribute: duration
chicago_bike_trip[, duration := .(difftime(ended_at, started_at, units = "min"))]

# Range constraint: remove entries with duration longer than a day 
day_in_mins <- 24*60
valid_duration_range <- c(0, day_in_mins )
chicago_bike_trip <- chicago_bike_trip[duration %between% valid_duration_range,]

# Remove outliers
iqr <- IQR(chicago_bike_trip[, duration])
lower_bound <- quantile(chicago_bike_trip[, duration], 0.25) - 1.5*iqr
upper_bound <- quantile(chicago_bike_trip[, duration], 0.75) + 1.5*iqr
chicago_bike_trip <- chicago_bike_trip[duration %between% c(lower_bound, upper_bound),]

# Remove invalid values: ids with 30 + characters 
chicago_bike_trip <- chicago_bike_trip[str_length(start_station_id) <= 30 | 
                                                     str_length(end_station_id) <= 30]
# Remove invalid values: ids and station names with empty entries 
chicago_bike_trip <- chicago_bike_trip[str_length(start_station_name) > 0 &  str_length(start_station_id) > 0] %>% 
  .[str_length(end_station_name) > 0 & str_length(end_station_id) > 0]
