# Launching the neccessary libraries for data manipulation.
library(tidyverse)
library(lubridate)
library(dplyr)
library(skimr)
library(janitor)

# Importing datasets.
daily_activity <- read.csv("cleaned_datasets/daily_activity_cleaned.csv")
daily_intensities <- read.csv("cleaned_datasets/daily_intensities_cleaned.csv")
sleep_day <- read.csv("cleaned_datasets/sleep_day_cleaned.csv")
weight <- read.csv("cleaned_datasets/weight_cleaned.csv")

# When we converted the data files to csv format earlier before in data cleaning process,
# we lost the correct data types for date columns since csv files cant hold metadata.
# We again need to convert the datas to their correct types before we start our manipulation.

daily_activity$ActivityDate <- ymd(daily_activity$ActivityDate)
daily_intensities$ActivityDay <- ymd(daily_intensities$ActivityDay)
sleep_day$SleepDay <- parse_date_time(sleep_day$SleepDay, orders = c("ymd", "ymd HMS"))
weight$Date <- ymd_hms(weight$Date)


# Lets start our manipulation with the daily_avtivity data frame. 
# The data frame includes several 'distance' rows in our analysis we wont need them so we will remove them now.
daily_activity <- daily_activity %>% 
  select(-LoggedActivitiesDistance, -VeryActiveDistance, -ModeratelyActiveDistance, -LightActiveDistance, -SedentaryActiveDistance)

# Lets create a column do determine the activity levels of the customers.
daily_activity <- daily_activity %>% 
  mutate(activitiy_level = case_when(
    Calories < 2000 ~ 'Inactive',
    Calories >= 2000 & Calories <= 2400 ~ 'Moderately Active',
    Calories > 2400 ~ 'Active'
  ))

daily_activity %>% count(activitiy_level)



# Now I would like to get a brief statistical summary of our dataset to understand it better.
summary_daily_activity <- daily_activity %>% 
  summarize(
    average_calories = mean(Calories),
    average_sedantary_minutes = mean(SedentaryMinutes),
    average_very_active_minutes = mean(VeryActiveMinutes),
    average_total_steps = mean(TotalSteps),
    average_total_distance = mean(TotalDistance),
    min_total_steps = min(TotalSteps),
    max_total_steps = max(TotalSteps),
    min_total_distance = min(TotalDistance),
    max_total_distance = max(TotalDistance)
  )

# Now lets move on to the daily_intensities table.

# This table contains its values in minutes format, but I would like to change it to 
# hours as it is easier for us to interpret it that way. 

daily_intensities <- daily_intensities %>% 
  mutate(SedantaryHours = round(SedentaryMinutes / 60, 2),
         LightlyActiveHours = round(LightlyActiveMinutes / 60, 2),
         FairlyActiveHours = round(FairlyActiveMinutes / 60, 2),
         VeryActiveHours = round(VeryActiveMinutes / 60, 2))

# We have created 4 different columns with hour format and rounded them to two decimal places
# for better reading. Now we will delete the minute columns as we no longer need them.

daily_intensities <- daily_intensities %>% 
  select(-SedentaryMinutes,
         -LightlyActiveMinutes,
         -FairlyActiveMinutes,
         -VeryActiveMinutes)

# Now I would like to get a brief summary of our table.

summary(daily_intensities$SedantaryHours)
summary(daily_intensities$LightlyActiveHours)
summary(daily_intensities$FairlyActiveHours)
summary(daily_intensities$VeryActiveHours)


# Lets move on to the sleep_day table and work on it.
# Similar to the table before, this one also has the values in minute format. Lets change them to hours.

sleep_day <- sleep_day %>% 
  mutate(TotalHoursAsleep = round(TotalMinutesAsleep/60,2),
         TotalTimeInBedHours = round(TotalTimeInBed/60,2))

# Now we can remove the minute columns just like before.

sleep_day <- sleep_day %>% select(-TotalMinutesAsleep)
sleep_day <- sleep_day %>% select(-TotalTimeInBed)

# We can find the time it took for customers to fall asleep by calculating 
# TotalTimeInBedHours - TotalHoursAsleep and creating a new column.

sleep_day <- sleep_day %>%  
  mutate(MinutesTookToSleep = TotalTimeInBedHours - TotalHoursAsleep)

sleep_day <- sleep_day %>%  
  mutate(MinutesTookToSleep = 60 * MinutesTookToSleep)

# Great! Now we now the time it took for each customer to fall asleep on each observation!
# Lastly lets get a brief summary of our table to better understand it.

summary(sleep_day$TotalHoursAsleep)
summary(sleep_day$TotalTimeInBedHours)
summary(sleep_day$MinutesTookToSleep)

# We can move on to the weight data frame.

# In this dataset there is a column named BMI. 
# We want to create a different column that shows the BMI category of the customer depending
# on their BMI score. For example 'Underwheight' 'Healthy' 'Overweight' 'Obese' etc.

weight <- weight %>% 
  mutate(BMI_category = case_when(
    BMI < 18.5 ~ 'Underweight',
    BMI >= 18.5 & BMI <= 24.9 ~ 'Healthy',
    BMI >= 25 & BMI <= 29.9 ~ 'Overweight',
    BMI >= 30 ~ 'Obese'
  ))


# Great! Now we have a new row that gives us the classification of BMI values.

# I wanted to check how many of the customers have manually reported their data,
# for us we can run a simple filter code with dplyr.

weight %>% filter(IsManualReport == TRUE) %>% nrow()
weight %>% filter(IsManualReport == FALSE) %>% nrow()

# Lets get a quick summary of our data frame.
summary(weight)
# And the frequencies of our BMI categories.
weight %>% count(BMI_category)

# Before finishing our manipulation, I want to check the structure of our data frames for date columns one again.

str(daily_activity$ActivityDate)
str(daily_intensities$ActivityDay)
str(sleep_day$SleepDay)
str(weight$Date)

# Great, the date formats are all correct. 

write.csv(daily_activity, "daily_activity_mpd.csv", row.names = FALSE)
write.csv(daily_intensities, "daily_intensities_mpd.csv", row.names = FALSE)
write.csv(sleep_day, "sleep_day_mpd.csv", row.names = FALSE)
write.csv(weight, "weight_mpd.csv", row.names = FALSE)

# We finished our manipulation by exporting the processed dataframes to another folder in csv file,
# for later use in analysis and data visualization.




