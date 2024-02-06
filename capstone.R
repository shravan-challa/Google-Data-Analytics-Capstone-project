#libraries
install.packages("tidyverse")
library(tidyverse) 
library(lubridate) #dates
library(readr) #load CSVs
library(ggplot2) #plotting
library(dplyr)

#disable exponential notation
options(scipen = 999) 

#Importing Data from 2021
jan <- read_csv("202101-divvy-tripdata.csv")
feb <- read_csv("202102-divvy-tripdata.csv")  
mar <- read_csv("202103-divvy-tripdata.csv")  
apr <- read_csv("202104-divvy-tripdata.csv")  
may <- read_csv("202105-divvy-tripdata.csv")
jun <- read_csv("202106-divvy-tripdata.csv")  
jul <- read_csv("202107-divvy-tripdata.csv")
aug <- read_csv("202108-divvy-tripdata.csv")  
sep <- read_csv("202109-divvy-tripdata.csv")
oct <- read_csv("202110-divvy-tripdata.csv")  
nov <- read_csv("202111-divvy-tripdata.csv")  
dec <- read_csv("202112-divvy-tripdata.csv")  


#Check column names for consistency
colnames(jan)
colnames(feb)
colnames(mar)
colnames(apr)
colnames(may)
colnames(jun)
colnames(jul)
colnames(aug)
colnames(sep)
colnames(oct)
colnames(nov)
colnames(dec)


#Merge monthly data into quarterly collection
#q1_df <- rbind (jan, feb, mar)
#q2_df <- rbind (apr, may, jun)
#q3_df <- rbind (jul, aug, sep)
#q4_df <- rbind (oct, nov, dec)
full_df <- rbind(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)


#Remove monthly data to clear environment
remove(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)


#Inspect dataframes and look for incongruencies 
#str(q1_df)
#str(q2_df)
#str(q3_df)
#str(q4_df)
str(full_df)


#Remove rows with missing data
#q1_df <- na.omit(q1_df)
#q2_df <- na.omit(q2_df)
#q3_df <- na.omit(q3_df)
#q4_df <- na.omit(q4_df)
full_df <- na.omit(full_df)


#Convert trip_duration to numeric to calculate correctly
#q1_df <- mutate(q1_df, trip_duration=as.numeric(trip_duration))
#q2_df <- mutate(q2_df, trip_duration=as.numeric(trip_duration))
#q3_df <- mutate(q3_df, trip_duration=as.numeric(trip_duration))
#q4_df <- mutate(q4_df, trip_duration=as.numeric(trip_duration))
full_df <- mutate(full_df, trip_duration=as.numeric(trip_duration))


#Full year data frame, clear quarterly data
#full_df <- rbind(q1_df, q2_df, q3_df, q4_df)
#remove(q1_df, q2_df, q3_df, q4_df)


#Plots

#Total rides per year by member/casual
full_df %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, 
             y = number_of_rides, 
             fill = member_casual)) + 
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Year', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Rider Group') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides), vjust = 0)


#Rides per month by member/casual
full_df$month <- ordered(full_df$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) #rearrange months to January -> December 
full_df %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, 
             y = number_of_rides, 
             fill = member_casual)) + 
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Month', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Month') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides, hjust = "left"), position = position_dodge(width = 0.9),size = 3, angle = 90)


#Rides per day by member/casual
full_df$weekday <- ordered(full_df$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) #rearrange weekdays to monday -> sunday 
full_df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, 
             y = number_of_rides, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Day', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Day of Week') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides, hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)


#Average trip duration by member/casual
full_df %>% 
  group_by(member_casual) %>% 
  summarise(average_duration = mean(trip_duration)) %>% 
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, 
             y = average_duration/60, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Rider Group') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2)), vjust = 0)
  
    
#Average trip duration per month by member/casual
full_df %>% 
  group_by(member_casual, month) %>% 
  summarise(average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, 
             y = average_duration/60, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration by Month', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Month') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2), hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)


#Average trip duration per weekday by member/casual
full_df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, 
             y = average_duration/60, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration by Weekday', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Day of Week') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2), hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)


#Bike Preference per Rider Group
full_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, 
             y = number_of_rides, 
             fill = member_casual)) + 
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Year by Bike Type', subtitle = 'Casual Riders vs Members') +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Bike Type') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides), position = position_dodge(width = 0.9), vjust = 0)


#Average trip duration per bike type
full_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, 
             y = average_duration/60, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Trip Duration by Bike Type', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Bike Type') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)
