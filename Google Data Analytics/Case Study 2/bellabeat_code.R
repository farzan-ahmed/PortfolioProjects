#install packages
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggrepel")

library(tidyverse)
library(ggpubr)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(ggrepel)

#import data
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

#preview datasets
head(daily_activity)
str(daily_activity)

head(daily_sleep)
str(daily_sleep)

head(hourly_steps)
str(hourly_steps)

#cleaning and formatting
n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(hourly_steps$Id)

#remove duplicate and NA values
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))

daily_activity <- daily_activity %>% 
  distinct() %>% 
  drop_na()
daily_sleep <- daily_sleep %>% 
  distinct() %>% 
  drop_na()
hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()

#clean and rename columns
clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)
clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)
clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

#consistency of date and time column
daily_activity <- daily_activity %>% 
  rename(date = activitydate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>% 
  rename(date = sleepday) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p" , tz = Sys.timezone()))

hourly_steps <- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
head(hourly_steps)

#merge daily_activity and daily_sleep datasets
daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c("id", "date"))
glimpse(daily_activity_sleep)

#daily steps average by user
daily_average <- daily_activity_sleep %>% 
  group_by(id) %>% 
  summarise(mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories),
            mean_daily_sleep = mean(totalminutesasleep))
head(daily_average)

#classify user type by daily average steps
user_type <- daily_average %>% 
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "Sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "Lightly Active",
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "Moderately Active",
    mean_daily_steps >= 10000 ~ "Very Active"
  ))
head(user_type)

#create user type data frame with user type percentage
user_type_percent <- user_type %>% 
  group_by(user_type) %>% 
  summarise(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(user_type) %>% 
  summarise(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

user_type_percent$user_type <- factor(user_type_percent$user_type,
                                      levels = c("Very Active", "Moderately Active", "Lightly Active", "Sedentary"))
head(user_type_percent)

#visualize user type distribution
user_type_percent %>% 
  ggplot(aes(x = "", y = total_percent, fill=user_type)) + geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + theme_minimal() + theme(axis.title.x = element_blank(), axis.line.y = element_blank(), 
                                                        panel.border = element_blank(), panel.grid = element_blank(), 
                                                        axis.ticks = element_blank(), axis.text.x = element_blank(), 
                                                        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  scale_fill_manual(values = c("#85e085", "#e6e600", "#ffd480", "#ff8080")) + 
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) + labs(title = "User Type Distribution") 

#steps and minutes asleep per weekday
weekday_steps_sleep <- daily_activity_sleep %>% 
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <- ordered(weekday_steps_sleep$weekday,
                                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <- weekday_steps_sleep %>% 
  group_by(weekday) %>% 
  summarize(daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))
head(weekday_steps_sleep)

#visualize steps and minutes asleep per weekday
ggarrange(
  ggplot(weekday_steps_sleep) + geom_col(aes(weekday, daily_steps, fill="#006699")) + 
    geom_hline(yintercept = 7500) + labs(title = "Daily Steps Per Weekday", x='', y="") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)),
  ggplot(weekday_steps_sleep) + geom_col(aes(weekday, daily_sleep, fill="#85e0e0")) + 
    geom_hline(yintercept = 480) + labs(title = "Minutes Asleep Per Weekday", x="", y="") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
)

#hourly steps throughout the day
hourly_steps <- hourly_steps %>% 
  separate(date_time, into = c("date","time"), sep = " ") %>% 
  mutate(date = ymd(date))
head(hourly_steps)

hourly_steps %>% 
  group_by(time) %>% 
  summarize(average_steps = mean(steptotal)) %>% 
  ggplot() + geom_col(aes(x=time, y=average_steps, fill=average_steps)) + 
  labs(title = "Hourly Steps Throughout The Day", x="", y="") + 
  scale_fill_gradient(low = "green", high = "red") + 
  theme(axis.text.x = element_text(angle = 90))
  
#visualize correlation between Daily Steps vs Daily Sleep & Daily Steps vs Daily Calories
ggarrange(
  ggplot(daily_activity_sleep, aes(x=totalsteps, y=totalminutesasleep)) + 
    geom_jitter() + geom_smooth(color = "red") + 
    labs(title = "Daily Steps vs Daily Sleep", x = "Daily Steps", y = "Daily Sleep") + 
    theme(panel.background = element_blank(), plot.title = element_blank()),
  ggplot(daily_activity_sleep, aes(x=totalsteps, y=calories)) + 
    geom_jitter() + geom_smooth(color = "red") + 
    labs(title = "Daily Steps vs Calories", x="Daily Steps", y="Calories") + 
    theme(panel.background = element_blank(), plot.title = element_blank())
)

#daily device usage by user
daily_use <- daily_activity_sleep %>% 
  group_by(id) %>% 
  summarize(days_used = sum(n())) %>% 
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "Low Usage",
    days_used >= 11 & days_used <= 20 ~ "Moderate Usage",
    days_used >= 21 & days_used <= 31 ~ "High Usage"
  ))
head(daily_use)

#create daily usage data frame with daily use percentage
daily_use_percent <- daily_use %>% 
  group_by(usage) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(usage) %>% 
  summarize(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

daily_use_percent$usage <- factor(daily_use_percent$usage,
                                  levels = c("High Usage",
                                             "Moderate Usage", 
                                             "Low Usage"))
head(daily_use_percent)

#visualize daily device use by user percentage
daily_use_percent %>% 
  ggplot(aes(x = "", y = total_percent, fill = usage)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(), 
        axis.ticks = element_blank(), axis.text.x = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) + 
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values = c("#006633","#00e673","#80ffbf"), 
                    labels = c("High Usage - 21 to 31 days", 
                               "Moderate Usage - 11 to 20 Days", 
                               "Low Usage - 1 to 10 Days")) + 
  labs(title = "Daily Device Usage")

#minutes device used by user per day
daily_use_merged <- merge(daily_activity, daily_use, by = c("id"))
head(daily_use_merged)

minutes_worn <- daily_use_merged %>% 
  mutate(total_minutes_worn = veryactiveminutes + fairlyactiveminutes + sedentaryminutes) %>% 
  mutate(percent_minutes_worn = (total_minutes_worn/1440)*100) %>% 
  mutate(worn = case_when(
    percent_minutes_worn == 100 ~ "All Day",
    percent_minutes_worn <= 100 & percent_minutes_worn >= 50 ~ "Half Day",
    percent_minutes_worn <= 50 & percent_minutes_worn >= 0 ~ "Less Than Half Day"
  ))
head(minutes_worn)

#create data frame by categories
minutes_worn_percent <- minutes_worn %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarize(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

minutes_worn_high_usage <- minutes_worn %>% 
  filter(usage == "High Usage") %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarize(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

minutes_worn_mod_usage <- minutes_worn %>% 
  filter(usage == "Moderate Usage") %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarize(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

minutes_worn_low_usage <- minutes_worn %>% 
  filter(usage == "Low Usage") %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarize(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

minutes_worn_high_usage$worn <- factor(minutes_worn_high_usage$worn, 
                                       levels = c("All Day", "Half Day", 
                                                  "Less Than Half Day"))
minutes_worn_mod_usage$worn <- factor(minutes_worn_mod_usage$worn, 
                                      levels = c("All Day", "Half Day", 
                                                 "Less Than Half Day"))
minutes_worn_low_usage$worn <- factor(minutes_worn_low_usage$worn, 
                                      levels = c("All Day", "Half Day", 
                                                 "Less Than Half Day"))

head(minutes_worn_percent)
head(minutes_worn_high_usage)
head(minutes_worn_mod_usage)
head(minutes_worn_low_usage)

#visualize data frames by categories
ggarrange(
  ggplot(minutes_worn_percent, aes(x="", y=total_percent, fill=worn)) + 
    geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0) + 
    theme_minimal() + theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(), 
                            panel.border = element_blank(),
                            panel.grid = element_blank(), 
                            axis.ticks = element_blank(), 
                            axis.text.x = element_blank(), 
                            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                            plot.subtitle = element_text(hjust = 0.5)) + 
    scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) + 
    geom_text(aes(label = labels), position = position_stack(vjust = 0.5), size = 3.5) + 
    labs(title = "Time Worn Per Day", subtitle = "Total Users"),
  ggarrange(
    ggplot(minutes_worn_high_usage, aes(x="", y=total_percent, fill=worn)) + 
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y", start = 0) + theme_minimal() + theme(axis.title.x = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            panel.border = element_blank(),
                                                            axis.ticks = element_blank(),
                                                            axis.text.x = element_blank(),
                                                            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                                            plot.subtitle = element_text(hjust = 0.5), 
                                                            legend.position = "none") + 
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) + geom_text_repel(aes(label = labels),
                                                                                       position = position_stack(vjust = 0.5), size = 3) + 
      labs(title = "", subtitle = "High Usage - Users"),
    ggplot(minutes_worn_mod_usage, aes(x="", y=total_percent, fill=worn)) + 
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y",start = 0) + theme_minimal() + theme(axis.title.x = element_blank(),
                                                           axis.title.y = element_blank(),
                                                           panel.border = element_blank(),
                                                           axis.ticks = element_blank(),
                                                           axis.text.x = element_blank(),
                                                           plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                                           plot.subtitle = element_text(hjust = 0.5),
                                                           legend.position = "none") + 
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) + geom_text_repel(aes(label = labels),
                                                                                       position = position_stack(vjust = 0.5), size = 3) + 
      labs(title = "", subtitle = "Moderate Usage - Users"),
    ggplot(minutes_worn_low_usage, aes(x="", y=total_percent, fill=worn)) + 
      geom_bar(stat = "identity", width = 1) + 
      coord_polar("y", start = 0) + theme_minimal() + theme(axis.title.x = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            panel.border = element_blank(),
                                                            axis.ticks = element_blank(),
                                                            axis.text.x = element_blank(),
                                                            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                                                            plot.subtitle = element_text(hjust = 0.5),
                                                            legend.position = "none") + 
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff")) + geom_text_repel(aes(label = labels),
                                                                                       position = position_stack(vjust = 0.5), size = 3) + 
      labs(title = "", subtitle = "Low Usage - Users")
  )
)

