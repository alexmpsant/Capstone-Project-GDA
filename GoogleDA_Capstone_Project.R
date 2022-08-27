
library(tidyverse)
library(lubridate)
library(skimr)
library(scales)

# options(dplyr.summarise.inform = FALSE)

tripdata_202108 <- read.csv("202108-divvy-tripdata.csv")
tripdata_202109 <- read.csv("202109-divvy-tripdata.csv")
tripdata_202110 <- read.csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("202112-divvy-tripdata.csv")
tripdata_202201 <- read.csv("202201-divvy-tripdata.csv")
tripdata_202202 <- read.csv("202202-divvy-tripdata.csv")
tripdata_202203 <- read.csv("202203-divvy-tripdata.csv")
tripdata_202204 <- read.csv("202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("202205-divvy-tripdata.csv")
tripdata_202206 <- read.csv("202206-divvy-tripdata.csv")
tripdata_202207 <- read.csv("202207-divvy-tripdata.csv")


str(tripdata_202108)
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)
str(tripdata_202201)
str(tripdata_202202)
str(tripdata_202203)
str(tripdata_202204)
str(tripdata_202205)
str(tripdata_202206)
str(tripdata_202207)




all_tripdata <- bind_rows(tripdata_202108,tripdata_202109,tripdata_202110,tripdata_202111,tripdata_202112,
                          tripdata_202201,tripdata_202202,tripdata_202203,tripdata_202204,
                          tripdata_202205,tripdata_202206,tripdata_202207)

head(all_tripdata)
glimpse(all_tripdata)
str(all_tripdata)
colnames(all_tripdata)
skim_without_charts(all_tripdata)


# Creating a new column for the trip start hour
all_tripdata <- all_tripdata %>%
  mutate(start_hour = hour(started_at))

# Add column for month (ride start)
all_tripdata$month <- format(as.Date(all_tripdata$started_at),'%y_%m')

# Add column for day of the week
all_tripdata <- all_tripdata %>% 
  mutate(day_of_week = wday(started_at,label=TRUE,abbr=FALSE))

# Modify columns as date type
all_tripdata <- all_tripdata %>%
  mutate(started_at = ymd_hms(started_at), ended_at = ymd_hms(ended_at))

# Add column for ride length
all_tripdata <- all_tripdata %>% 
  mutate(ride_length = difftime(ended_at, started_at, tz, units="mins"))

# check ride_length < 0
nrow(subset(all_tripdata,ride_length < 0))

# remove ride_length < 0
all_tripdata <- all_tripdata[!(all_tripdata$ride_length < 0),]

# rename columns
all_tripdata <- all_tripdata %>%
  rename(customer_type = member_casual)

# remove unused columns
all_tripdata <- all_tripdata %>%
  select(-c(ride_id,start_station_name,start_station_id,end_station_name,
              end_station_id,start_lat,start_lng,end_lat,end_lng))


##############################################  Ride Type Vs. Costumers  ##############################################




ggplot(all_tripdata, aes(customer_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="purple")+ 
  scale_y_continuous(label=scales::percent)+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25)+
  labs(title = "Total Rides Vs.Customer Type ",
    x = "Customer Type",
    y = "Total Rides(%)")
 

all_tripdata %>% 
  ggplot() + geom_bar(aes(x=rideable_type, fill=rideable_type)) + 
  facet_wrap(~customer_type) + labs(x="Ride Type", y="Total Rides")+
  labs(title ="Ride Type and Total Rides per. Costumer Type")

all_tripdata %>% 
  ggplot(aes(x=customer_type,y=ride_length,color=rideable_type)) + 
  geom_boxplot() + labs(x="Costumer Type",y="Ride length (min.)")+
  labs(title ="Ride length (min.) per Costumer Type")+
  scale_y_continuous(name="Ride lenth (min.)", limits=c(0, 100))




##############################################  Daily bike usage  ##############################################



all_tripdata %>%
  group_by(start_hour, customer_type)%>%
  summarise(total_rides = n()) %>%
  ggplot(aes(x=start_hour, y= total_rides, group = customer_type, color = customer_type))+
  geom_line() + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))+
  labs(title = "Daily Bike usage per Customer type", x = "Time of Day")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
 

all_tripdata %>%
  group_by(start_hour, customer_type)%>%
  summarise(avg_ride_duration =  mean(ride_length)) %>%
  ggplot(aes(x=start_hour, y= avg_ride_duration, group = customer_type, color = customer_type))+
  geom_line() + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))+
  labs(title = "Daily Bike usage per Avg. Ride Duration (min.)", x = "Time of Day")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


################################################################################################################

##############################################  Weekly bike usage  #############################################

all_tripdata %>%  
  group_by(customer_type, day_of_week) %>% 
  summarise(total_rides = n()) %>% 
  arrange(customer_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = total_rides, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Total Rides over the week per Customer type ")+
  scale_fill_manual("customer_type", values = c("member" = 'purple', 'casual' = 'orange')) 


all_tripdata %>%
  group_by(customer_type, day_of_week) %>%
  summarise(avg_ride_duration =  mean(ride_length)) %>%
  ggplot(aes(x=day_of_week, y= avg_ride_duration, fill= customer_type)) + 
  geom_col(width = 0.5, position = position_dodge(width = 0.5))+
  labs(title= "Average ride duration (minutes) by type of Customer per Day of the week") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  scale_fill_manual("customer_type", values = c("member" = 'purple', 'casual' = 'orange')) 

################################################################################################################

##############################################  Monthly bike usage  #############################################
all_tripdata %>%  
  group_by(customer_type, month) %>% 
  summarise(total_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = total_rides, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Total Rides over the months per Customer type ")+
  scale_fill_manual("customer_type", values = c("member" = 'purple', 'casual' = 'orange')) 


all_tripdata %>%
  group_by(customer_type, month) %>%
  summarise(avg_ride_duration =  mean(ride_length)) %>%
  ggplot(aes(x = month, y= avg_ride_duration, fill= customer_type)) + 
  geom_col(width = 0.5, position = position_dodge(width = 0.5))+
  labs(title= "Average ride duration (minutes) by type of Customer over the months") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  scale_fill_manual("customer_type", values = c("member" = 'purple', 'casual' = 'orange')) 



