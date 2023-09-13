title: "Belladata Case Study"
author: "Lyzet Flores"
date: "2023-09-09"
output: html_document


#installing packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('skimr')

#loading the packages
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)

#df_name <- read.csv(Fitabase Data 4.12.16-5.12.16)
daily_activity <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_sleep <- read.csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log <- read.csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# averages we found in the data

summary(daily_activity_cleaned$total_steps)
summary(daily_activity_cleaned$very_active_minutes)
summary(daily_activity_cleaned$sedentary_hours)
summary(daily_sleep$hours_asleep)

#we can see that the average steps per dat were 8,319, which is more than the recommended6000-8000
#the average sedentary hours were 23.21 which is less than the recommended 30 minutes
#the average sedentary hours were 15.87 which is much more than the recommended 10 hours
#the average hours spent asleep was 6.9 which is cutting it short to the recommended 7 hours of sleep

#str lets take a look at the data

str(daily_activity)
str(daily_sleep)
str(weight_log)

#To clean up the column names

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
weight_log <- clean_names(weight_log)

#clean up the data format inteh sleep activity data

daily_activity$activity_date <- as.Date(daily_activity$activity_date,'%m/%d/%y')
daily_sleep$sleep_day <- as.Date(daily_sleep$sleep_day, '%m/%d/%y')

#change the format for weight log

weight_log$date <- parse_date_time(weight_log$date, '%m/%d/%y %H:%M:%S %p')
weight_log$is_manual_report <- as.logical(weight_log$is_manual_report)
#check the weight log data
str(weight_log)

#I will take off weight_log fat aspect because it does not give much context and is not helpful to the analysis.
weight_log <- weight_log %>%
  select(-c(fat))

daily_activity$day_of_week <- wday(daily_activity$activity_date, label = T, abbr = T)
daily_activity$total_active_hours = round((daily_activity$very_active_minutes + daily_activity$fairly_active_minutes + daily_activity$lightly_active_minutes)/60, digits = 2)
daily_activity$sedentary_hours = round((daily_activity$sedentary_minutes)/60, digits = 2)

daily_sleep$hours_in_bed = round((daily_sleep$total_time_in_bed)/60, digits = 2)
daily_sleep$hours_asleep = round((daily_sleep$total_minutes_asleep)/60, digits = 2)
daily_sleep$time_taken_to_sleep = (daily_sleep$total_time_in_bed - daily_sleep$total_minutes_asleep)

daily_activity_cleaned <- daily_activity[!(daily_activity$calories<=0),]
daily_activity_cleaned <- daily_activity_cleaned[!(daily_activity_cleaned$total_active_hours<=0.00),]

#visualizing some trends in the data

options(scippen =999)
ggplot(data = daily_activity_cleaned) +
  aes(x = day_of_week, y = total_steps) +
  geom_col(fill =  'dark green') +
  labs(x = 'Day of the week', y = 'Total steps', title = 'Total steps taken in a week')
ggsave('total_steps.png')


ggplot(data = daily_activity_cleaned) +
  aes(x=day_of_week, y= calories) +
  geom_col(fill = 'blue') +
  labs(x='Day of the week', y= 'Calories burned', title = 'Total calories burned in a week')
ggsave('total_calories.png')

ggplot(data = daily_activity_cleaned) +
  aes(x = day_of_week, y = very_active_minutes) +
  geom_col(fill =  'red') +
  labs(x = 'Day of the week', y = 'Total of very active minutes', title = 'Total activity in a week')
ggsave('total_activity.png')

#now that we see Sunday was the day with the most activity lets see how steps and calories relate
ggplot(data = daily_activity_cleaned)+
  aes(x=total_steps, y=calories)+
  geom_point(color ='purple')+
  geom_smooth() +
  labs( x = 'Total steps', y = 'Calories burned', title = 'Calories burned vs. total steps')
 ggsave('calories_burned_vs_total_steps.png') 

ggplot(data = daily_activity_cleaned) +
  aes(x= total_active_hours, y = calories) +
  geom_point(color = 'orange') +
  geom_smooth() +
  labs(x = 'Total active hours', y = 'Calories burned', title = 'Calories burned vs active hours')
ggsave('calories_burned_vs_total_steps.png')

ggplot(data = daily_activity_cleaned) +
  aes(x= sedentary_hours, y = calories) +
  geom_point(color = 'red') +
  geom_smooth() +
  labs(x = 'Sedentary hours', y = 'Calories burned', title = 'Calories burned vs sedentary hours')
ggsave('sedentary_hours_vs_calories_burned.png')

#I want to see the relationship between the weight and physical activity

activity_weight <- merge(daily_activity_cleaned,weight_log, by=c('id'))

ggplot(data = activity_weight)+
  aes(x =very_active_minutes, y = weight_pounds) +
  geom_smooth(fill ='pink') +
  labs(x='Very active minutes', y = 'Weight(lbs)', title ='Relationship between weight and physical activity')
ggsave('relationship_weight_physical_activity.png')

ggplot(data = activity_weight)+
  aes(x =total_steps, y = weight_pounds) +
  geom_smooth(fill ='red') +
  labs(x='Total steps', y = 'Weight(lbs)', title ='Relationship between weight and physical activity')
ggsave('relationship_weight_and_steps.png')

##This concludes my analysis of the Bellabeat data!
##I have made a Google slide presentation that I have added to my Github page. Thank you!





