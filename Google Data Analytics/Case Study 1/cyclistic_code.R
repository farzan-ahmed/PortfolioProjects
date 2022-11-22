#Installing Packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

#Importing Data
trip_data_202112 <- read_csv("202112-divvy-tripdata.csv")
trip_data_202201 <- read_csv("202201-divvy-tripdata.csv")
trip_data_202202 <- read_csv("202202-divvy-tripdata.csv")
trip_data_202203 <- read_csv("202203-divvy-tripdata.csv")
trip_data_202204 <- read_csv("202204-divvy-tripdata.csv")
trip_data_202205 <- read.csv("202205-divvy-tripdata.csv")
trip_data_202206 <- read.csv("202206-divvy-tripdata.csv")
trip_data_202207 <- read.csv("202207-divvy-tripdata.csv")
trip_data_202208 <- read.csv("202208-divvy-tripdata.csv")
trip_data_202209 <- read.csv("202209-divvy-publictripdata.csv")
trip_data_202210 <- read.csv("202210-divvy-tripdata.csv")

str(trip_data_202112)
str(trip_data_202201)
str(trip_data_202202)
str(trip_data_202203)
str(trip_data_202204)
str(trip_data_202205)
str(trip_data_202206)
str(trip_data_202207)
str(trip_data_202208)
str(trip_data_202209)
str(trip_data_202210)

#Combining Data
divvy_data <- rbind(trip_data_202112, trip_data_202201, trip_data_202202, trip_data_202203, trip_data_202204, trip_data_202205,
                    trip_data_202206, trip_data_202207, trip_data_202208, trip_data_202209, trip_data_202210)
head(divvy_data)

#Clean Data
divvy_data$date <- as.Date(divvy_data$started_at)
divvy_data$month <- format(as.Date(divvy_data$date), "%b")
divvy_data$day <- format(as.Date(divvy_data$date), "%d")
divvy_data$year <- format(as.Date(divvy_data$date), "%Y")
divvy_data$day_of_week <- format(as.Date(divvy_data$date), "%A")

head(divvy_data)

#Removing NA's
divvy_data <- drop_na(divvy_data)

#Remove duplicates from dataframes
divvy_data <- divvy_data[!duplicated(divvy_data$ride_id), ]
print(paste("Removed", nrow(divvy_data) - nrow(divvy_data), "duplicate rows"))

#Create column to determine ride length
divvy_data_v2 <- mutate(divvy_data, ride_length = difftime(ended_at, started_at, units = "mins"))
str(divvy_data_v2)

#Filtering out with rides less than 0
nrow(divvy_data_v2[divvy_data_v2$ride_length < 0,])
divvy_data_v3 <- divvy_data_v2[!divvy_data_v2$ride_length < 0,]
glimpse(divvy_data_v3)

#determining amount of members vs casual riders
rider_type_total <- table(divvy_data_v3$member_casual)
View(rider_type_total)

#statistical analysis
trip_stats <- divvy_data_v3 %>% 
  group_by(member_casual) %>% 
  summarize(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length),
            median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_stats)

#determine for the day of the week
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(divvy_data_v3$day_of_week)
print(weekday_mode)

#determine the most popular day by rider type
divvy_data_v3$day_of_week <- ordered(divvy_data_v3$day_of_week,
                                     levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

divvy_data_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(rider_type_total = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

#determine the most popular month
popular_month <- divvy_data_v3 %>% 
  group_by(month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(-number_of_rides)
view(popular_month)

#determine the most popular start station
station_mode <- getmode(divvy_data_v3$start_station_name)
print(station_mode)

#determine most popular station for member
popular_start_stations_member <- divvy_data_v3 %>% 
  filter(member_casual == "member") %>% 
  group_by(start_station_name) %>% 
  summarize(number_of_starts = n()) %>% 
  filter(start_station_name != "" ) %>% 
  arrange(-number_of_starts)
head(popular_start_stations_member)

#determine most popular station for casual riders
popular_start_stations_casual_riders <- divvy_data_v3 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarize(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(-number_of_starts)
head(popular_start_stations_casual_riders)

#visualization of rider types
divvy_data_v3 %>% 
  group_by(member_casual) %>% 
  summarize(total_rider_type = n()) %>% 
  ggplot(aes(x=member_casual, y=total_rider_type, fill=member_casual)) +
  geom_col(position = "dodge") + geom_text(aes(label = total_rider_type, vjust=-0.25))

#visualization of rider types ride duration
rider_type_average_duration <- divvy_data_v3 %>% 
  group_by(member_casual) %>% 
  summarize(average_ride_length = mean(ride_length))
rider_type_average_duration %>% 
  ggplot(aes(x=member_casual, y=average_ride_length, fill=member_casual)) +
  geom_col(position = "dodge") + geom_text(aes(label = average_ride_length, vjust=0.1))

#visualization of usage by riders type by the week
divvy_data_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x=day_of_week, y=average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") #+ theme(axis.text.x = element_text(angle = 45))

#visualization of number of riders by rider type by the week
divvy_data_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x=day_of_week, y=number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge")

#visualization of the usage by members and casual riders by the month
divvy_data_v3$month <- ordered(divvy_data_v3$month,
                               levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
divvy_data_v3 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=average_duration, fill=member_casual)) + 
  geom_col(position = "dodge") + geom_text(aes(label = number_of_rides, angle = 90)) + 
  facet_wrap(~member_casual)

#visualization of the number of trips by members and casual riders by the month
divvy_data_v3 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") + geom_text(aes(label = number_of_rides, angle =90)) +
  facet_wrap(~member_casual)

# Saving clean data frame as CSV so it can be uploaded to tableau for additional visualisation 
divvy_data_v3 %>%
  write.csv("divvy_data_v3.csv")
