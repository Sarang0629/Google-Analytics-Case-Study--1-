#1. Installing Packages:
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("lubridate")
install.packages("scales")
install.packages("geosphere")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)
library(lubridate)
library(scales)
library(geosphere)

#2. Importing Datasets & Merging them into One single dataframe:
####12-Months Datasets are extracted and stored in the Folder- Trip_csv.
####The Datasets then imported and combined to create one single data frame named - Trip.

Trip = list.files(path='D:/Capstone-Case Study 1/Trip_csv', full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

####After combining the datasets the data frame looks like 
head(Trip)

####After looking the Trip Dataframe we notice that it contains na values which we will remove to avoid inconsistency in our data
sapply(Trip, function(x) sum(is.na(x)))
Trip1 = na.omit(Trip)
View(Trip1)

#3. Data Analysis & Visualization

#### Number of Rides VS Type of Riders
ggplot(Trip1, aes(x=member_casual))+geom_bar(fill = "skyblue")+
  labs(title = "Graph-1: Rides completed by Type of Riders",x = "Rider Type",y = "Number of rides")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  geom_text(stat='count', aes(label=..count..), color="black",vjust=15)

  
####  Total distance (in kilometers) traveled by user type
#####Creating new feature or Column named=distance using longitude and latitude given inside the dataframe
  
Trip1 = Trip1 %>%
  mutate(Trip1, distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))/1000)

Trip2 = Trip1 %>%
  group_by(member_casual) %>%
  summarise(distance=sum(distance)) 
  
ggplot(Trip2, aes(x=member_casual, y=distance)) +
  geom_bar(stat = "identity", fill= "navyblue") +
  labs(title = "Graph-2:Distance travelled by user type",x = "Rider type",y = "Distance Travelled in Km") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 2e-6)) +
  geom_text(aes(label=round(stat(y),2)), color="white",vjust=15)

Trip2.1 =Trip1%>% 
  group_by(member_casual) %>%
  summarise(average_distance_km=mean(distance)) 
print(Trip2.1)

#####  Duration of Riders Rode the Bike

Trip1 = Trip1 %>% 
  mutate(Trip1, time_difference = difftime(ended_at, started_at, units = "hours"))
  
Trip3 = Trip1 %>%
  group_by(member_casual) %>%
  summarise(duration=sum(time_difference))
  
  
ggplot(Trip3, aes(x=member_casual, y=duration)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Graph-3: Duration of Riders Rode the Bike",x = "Rider Type",y = "Hours cycled") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  geom_text(aes(label=round(duration,2)), vjust=15, color="white")

Trip3.1 = Trip1 %>%
  group_by(member_casual) %>%
  summarise(average_time=mean(time_difference))
print(Trip3.1)

####   Bike Preference by The Rider Type

Trip4 = Trip1 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(total_number_of_Riders = n())

ggplot(Trip4, aes(x=member_casual, y= total_number_of_Riders, fill=rideable_type)) +
  geom_bar(stat="identity") +
  labs(title = "Graph-4: Bike preference by user type",
    x = "Rider type",y = "Number of users") +
  geom_text(aes(label=total_number_of_Riders), position = position_stack(vjust = .5), color="white") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))


####Number of Rides Completed Month-wise by Rider Type


Trip5 = Trip1 %>% 
  mutate(Trip1, start_month_year = floor_date(as_date(started_at), "month")) %>%
  group_by(start_month_year, member_casual) %>%
  summarise(number_of_rides = n())

ggplot(Trip5, aes(x=start_month_year, y=number_of_rides, fill=member_casual))+
  geom_bar(stat="identity") +
  labs(title = "Graph-5: Number of Rides Completed Per Month VS Rider Type",x = "Month",y = "Number of Rides") +
  geom_text(aes(label=number_of_rides), position = position_stack(vjust = .5), color="white", angle = 90) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") + coord_flip()
  
#### Number of Rides Completed: Day VS Rider Type

Trip6 =Trip1 %>% 
  mutate(Trip1, day = weekdays(started_at)) %>%
  group_by(day, member_casual) %>%
  summarise(Number_of_Rides = n())

week = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

ggplot(Trip6, aes(x=factor(day, level = week), y=Number_of_Rides, fill=member_casual))+
  geom_bar(stat="identity") +
  labs(title = "Graph-6: Number of Rides Completed: Day VS Rider Type",x = "Day",y = "Number of Rides Completed",fill = "User type") +
  geom_text(aes(label=Number_of_Rides), position = position_stack(vjust = .5), color="black", angle = 90) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))

#### Finding out Top5 Stations 
T = Trip1 %>%
  group_by(member_casual, start_station_name) %>%
  summarise(number_rides=n(),.groups ='drop' ) %>%
  arrange(desc(number_rides))
View(T)
T2 = filter(T,member_casual == 'member') %>% slice(1:5)
head(T2)

T3 = filter(T,member_casual == 'casual') %>% slice(1:5)
View(T3)

