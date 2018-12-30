
library(tidyverse)
library(readxl)
library(leaflet)

## Functions 

max_consequtive_days <- function(data, station, year){
  threshold_exceeded_consequtive <- data %>%
    filter(Station_Name == station & Year == year) %>%
    mutate(threshold = if_else(sda_Max >= 18, 1, 0, missing = 0))
  
  rle_object <- rle(threshold_exceeded_consequtive$threshold)
  return(with(rle_object, max(lengths[values==1])))
}

threshold_counter <- function(data, station, year){
  counter <- data %>%
    filter(Station_Name == station & sda_Max > 18 & Year == year) %>%
    summarize(n = n())
  return(counter$n)
}

## Read in Data

station_info <- read_excel("TempMon_StationInfo.xlsx") 

daily_stats <- read_csv("LWC_temp_dash/AIIT_DailyStats.csv",
                        col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                         Time_Max = col_time(format = "%H:%M:%S"), 
                                         Time_Min = col_time(format = "%H:%M:%S")))

## Calculate threshold metrics: days over 18c, consecutive days over 18C.

temp_threshold_days <- daily_stats %>%
  group_by(Station_ID, Year) %>%
  filter(sda_Max > 18) %>%
  summarise(days_exceeded = n())

temp_threshold_max_consecutive <- daily_stats %>%
  group_by(Station_ID, Year) %>%
  mutate(exceeded = if_else(sda_Max >= 18, 1, 0, missing = 0)) %>%
  mutate(counter = sequence(rle(exceeded)$lengths)) %>%
  filter(exceeded == 1) %>%
  summarize(max_consecutive_exceeded = max(counter))


# Build leaflet object

leaflet(station_info) %>%
  addCircles(lng = ~Decimal_Longitude, lat = ~Decimal_Latitude) %>%
  addTiles()


  