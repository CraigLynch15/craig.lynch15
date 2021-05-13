library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

##Combining into yearly data sets

DF_2021 <- rbind(`202101.divvy.tripdata.2`, `202102.divvy.tripdata`, `202103.divvy.tripdata`)

DF_2020 <- rbind(`202004.divvy.tripdata`, `202005.divvy.tripdata`, `202006.divvy.tripdata`, `202007.divvy.tripdata`,
                 `202008.divvy.tripdata`, `202009.divvy.tripdata`, `202010.divvy.tripdata`,
                 `202011.divvy.tripdata`, `202012.divvy.tripdata`)

##Combining data sets into previous 12 months

DF_Recent <- rbind(DF_2021, DF_2020)

##Standardizing usertype names

DF_Recent <- DF_Recent %>%
  mutate(usertype = recode(member_casual
                           ,"Subscriber" = "member"
                           , "Customer" = "casual"))

##Deleting lat/long and rideable_type columns

DF_Recent_01 <- subset(DF_Recent, select = -c(member_casual, rideable_type,start_lat, start_lng, end_lat, end_lng))

##Creating a trip length column and ordering columns

DF_Recent_01$trip_length <- difftime(DF_Recent_01$ended_at,DF_Recent_01$started_at)


DF_Recent_Ordered <- DF_Recent_01[c(1,2,3,9,8,4,5,6,7)]

names(DF_Recent_Ordered)[1] <- "trip_id"
names(DF_Recent_Ordered)[2] <- "start_time"
names(DF_Recent_Ordered)[3] <- "end_time"
names(DF_Recent_Ordered)[4] <- "trip_length_sec"
names(DF_Recent_Ordered)[5] <- "usertype"
names(DF_Recent_Ordered)[6] <- "start_station_name"
names(DF_Recent_Ordered)[7] <- "start_station_id"
names(DF_Recent_Ordered)[8] <- "end_station_name"
names(DF_Recent_Ordered)[9] <- "end_station_id"

##Eliminating entries where trip_length < 60sec and > 86,400sec (24hours) as they are probably mistaken trips and 

DF_Recent_Ordered_01 <- filter(DF_Recent_Ordered, trip_length_sec >=60 & trip_length_sec <86400) 

##Separating start_time column into date and time

DF_Time <- separate(DF_Recent_Ordered_01, start_time, c("date", "time"), sep = " ")

##Making columns for day, month, and year

DF_Time$date <- as.Date(DF_Time$date) 
DF_Time$month <- format(as.Date(DF_Time$date), "%B")
DF_Time$day_of_week <- format(as.Date(DF_Time$date), "%A")

##Total Descriptive Analysis of Trip Length

median(DF_Time$trip_length_sec)
mean(DF_Time$trip_length_sec)
min(DF_Time$trip_length_sec)
max(DF_Time$trip_length_sec)

##Comparing members to casual riders

aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype, FUN = mean)
aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype, FUN = median)
aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype, FUN = max)
aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype, FUN = min)

##Comparing members to casual riders by day

DF_Time$day_of_week <- factor(DF_Time$day_of_week, levels= c("Monday", "Tuesday", "Wednesday", 
                                                             "Thursday", "Friday", "Saturday", "Sunday"))

DF_Time$day_of_week <- ordered(DF_Time$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype + DF_Time$day_of_week, FUN = mean)

##Comparing members to casual riders by month

DF_Time$month <- factor(DF_Time$month, levels= c("April", "May", "June", "July", "August", 
                                                 "September", "October", "November", "December",
                                                 "January", "February", "March"))

aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype + DF_Time$month, FUN = mean)

##Viz: number of rides by usertype

DF_Time %>% 
  group_by(usertype, day_of_week) %>%  
  summarise(number_of_rides = n()						
            ,average_duration = mean(trip_length_sec)) %>% 		
  arrange(day_of_week, usertype)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

DF_Time %>% 
  group_by(usertype, month) %>%  
  summarise(number_of_rides = n()						
            ,average_duration = mean(trip_length_sec)) %>% 		
  arrange(month, usertype)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45)) +
  ylim(0, 400000)

##Viz by trip length

DF_Time %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_length_sec)) %>% 
  arrange(usertype, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")


##Save simple table

counts <- aggregate(DF_Time$trip_length_sec ~ DF_Time$usertype + DF_Time$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/avg_ride_length.csv')

##Trying to group by time

ggplot(data = DF_Time, aes(x = trip_id, y = time)) +
  geom_point() 

