# Launching the necessery libraries for data cleaning.
library(tidyverse)
library(lubridate)
library(dplyr)
library(skimr)
library(janitor)


# Importing datasets.
daily_activity <- read.csv("datasets/dailyActivity_merged.csv")
daily_intensities <- read.csv("datasets/dailyIntensities_mergedd.csv")
sleep_day <- read.csv("datasets/sleepDay_mergedd.csv")
weight <- read.csv("datasets/weightLogInfo_mergedd.csv")


# Taking a look into our datasets.
glimpse(daily_activity)
glimpse(daily_intensities)
glimpse(sleep_day)
glimpse(weight)

# Taking a look into the distinct values in our dataset.
print(n_distinct(daily_activity$Id))
print(n_distinct(daily_intensities$Id))
print(n_distinct(sleep_day$Id))
print(n_distinct(weight$Id))


# Now we will only focus on the daily_activity table and check for whats there to clean.
# Checking if there is any missing values in our table.
missing_values <- colSums(is.na(daily_activity))
print(missing_values)

# Check for duplicates.
duplicates <- duplicated(daily_activity)
print(sum(duplicates))

# Check for the class of the date column, standardize.
print(class(daily_activity$ActivityDate))
daily_activity$ActivityDate <- parse_date_time(daily_activity$ActivityDate, orders = c("mdy", "dmy", "ymd"))
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate)
print(class(daily_activity$ActivityDate))
str(daily_activity)

# Afer checking our table we realise that the columns 'TotalDistance' and 'TrackerDistance'
# consist of the same values, we can drop one of the columns to remove clutter.
cleaned_daily_activity <- daily_activity %>% 
  select(-TrackerDistance)

# Now we can drop the old table.
rm(daily_activity)

# Our table was already mostly clean, and now we also correctly formatted the Date column and
# checked the rest of the columns for verifying the correct object types. 


# Now we will focus on the daily_intensities table for the same cleaning process.
missing_values <- colSums(is.na(daily_intensities))
print(missing_values)

duplicates <- duplicated(daily_intensities)
print(sum(duplicates))

# The ActivityDate column is not consistent throughout our table, so we will work on this.
daily_intensities$ActivityDay <- parse_date_time(daily_intensities$ActivityDay, orders = c("mdy", "dmy", "ymd"))

# Now we will change the object type do 'date'.
daily_intensities$ActivityDay <- as.Date(daily_intensities$ActivityDay)
print(class(daily_intensities$ActivityDay))

# In the table, we dont really need the 'distance' columns, so now we will get rid of them.
cleaned_daily_intensities <- daily_intensities %>% 
  select(-SedentaryActiveDistance, -LightActiveDistance, -ModeratelyActiveDistance, -VeryActiveDistance)

# We can drop the old daily_intensities table now.
rm(daily_intensities)

# We can move on to the sleep_day table.
# Start by checking null values again.
missing_values <- colSums(is.na(sleep_day))
print(missing_values)

# Check for duplicates.
duplicates <- duplicated(sleep_day)
print(sum(duplicates))

# We have found a total of 3 duplicates rows, now lets remove these.
sleep_day <- sleep_day[!duplicated(sleep_day), ]

# Now lets check again for duplicates.
print(sum(duplicated(sleep_day)))
# We have succesfully removed all 3 duplicate rows from our data frame.

# In this data frame too the sleepdate(date) column is poorly formatted, consisting of different 
# date-time formats, now lets take a look at this.

str(sleep_day)
sleep_day$SleepDay <- parse_date_time(sleep_day$SleepDay, orders = c("mdy HMS", "mdy HM", "mdy IMp", "mdy"))
sleep_day$SleepDay <- parse_date_time(sleep_day$SleepDay, orders = c("ymd", "ymd HMS"))
str(sleep_day$SleepDay)

# Now our date column is correctly formatted into date-time(POSIXct) format, and we cleaned the data frame.

# Similar cleaning process must be done for the weight data frame as well.
missing_values <- colSums(is.na(weight))
print(missing_values)

# We can see that the column 'Fat' has 65 missing values, and in total we have 67 records.
print(nrow(weight))
# In this case we can remove the 'Fat' column all together since 65/67 values are missing, meaning the column is nearly empty.
# I also choose to remove the LogId column as it wont be any use for us in our analysis.

weight <- weight %>% select(-Fat)
weight <- weight %>% select(-LogId)

# Moving on to checking duplicates.
duplicates <- duplicated(weight)
print(sum(duplicates))

# Correcting the date format in our 'Date' column.
weight$Date <- parse_date_time(weight$Date, orders = c("mdy HMS", "mdy HM", "mdy IMp", "mdy"))
str(weight$Date)
# The 'Date' column has been standardized. 

# Data cleaning process has been completed for all 4 tables.

# We can export our cleaned datasets to csv file format for later use.
write.csv(cleaned_daily_activity, "daily_activity_cleaned.csv", row.names = FALSE)
write.csv(cleaned_daily_intensities, "daily_intensities_cleaned.csv", row.names = FALSE)
write.csv(sleep_day, "sleep_day_cleaned.csv", row.names = FALSE)
write.csv(weight, "weight_cleaned.csv", row.names = FALSE)








