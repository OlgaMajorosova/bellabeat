# When preparing this case study I was inspired by Miguel FZZZ's case study published on kaggle. It looks great!
# I am not able yet to go to the same level of details (yet :-) ) but my goal is to find the following 3 correlations, between:

## Daily steps vs daily calories
## Daily steps vs sleep quality
## Daily steps vs weight (kg)


install.packages('tidyverse')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('kableExtra')
install.packages('scales')
install.packages('highcharter')
install.packages('RColorBrewer')
install.packages('readr')


library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)
library(kableExtra)   # For viewing tables
library(scales)   # For transforming numbers in percentage
library(highcharter) # Cool and interactive graphs
library(RColorBrewer)  # Pallete colors
library(readr)

daily_activity <- read_csv("C:/Users/Olga/Desktop/Google_case_study/Bellabeat/bellabeat/Data/dailyActivity_merged.csv")
daily_sleep <- read_csv("C:/Users/Olga/Desktop/Google_case_study/Bellabeat/bellabeat/Data/sleepDay_merged.csv")
weight_info <- read_csv("C:/Users/Olga/Desktop/Google_case_study/Bellabeat/bellabeat/Data/weightLogInfo_merged.csv")

kbl(daily_activity[1:5, ], 'html', caption = 'Table 1: Daily activity') %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% 
  scroll_box(width = "100%")  # Adding a scroll bar

kbl(daily_sleep[1:5, ], 'html', caption = 'Table 5: Daily sleep') %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

kbl(weight_info[1:5, ], 'html', caption = 'Table 6: Weight info') %>% 
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)

daily_activity <- daily_activity %>% 
  rename(Date = ActivityDate) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

daily_sleep <- daily_sleep %>% 
  rename(Date = SleepDay) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

weight_info <- weight_info %>% 
  select(-LogId) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  mutate(IsManualReport = as.factor(IsManualReport))

final_df <- merge(merge(daily_activity, daily_sleep, by = c('Id', 'Date'), all = TRUE), weight_info, by = c('Id', 'Date'), all = TRUE)

final_df <- final_df %>% select(-c(TrackerDistance, LoggedActivitiesDistance, TotalSleepRecords, WeightPounds, Fat, BMI, IsManualReport))

str(final_df)

summary(final_df)

custom_theme <- function() 
  theme(
    panel.border = element_rect(colour = "black", 
                                fill = NA, 
                                linetype = 1),
    panel.background = element_rect(fill = "white", 
                                    color = 'grey50'),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(colour = "black", 
                             face = "italic", 
                             family = "Helvetica"),
    axis.title = element_text(colour = "black", 
                              family = "Helvetica"),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(size=23, 
                              hjust = 0.5, 
                              family = "Helvetica"),
    plot.subtitle=element_text(size=16, 
                               hjust = 0.5),
    plot.caption = element_text(colour = "black", 
                                face = "italic", 
                                family = "Helvetica"))


## Daily steps vs daily calories
final_df %>% 
  group_by(TotalSteps, Calories) %>% 
  ggplot(aes(x = TotalSteps, y = Calories, color = Calories)) +
  geom_point() +
  geom_smooth() + 
  custom_theme() +
  theme(legend.position = c(.8, .3),
        legend.spacing.y = unit(1, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  labs(title = 'Daily steps vs daily calories',
       y = 'Calories',
       x = 'Total Steps',
       caption = 'Data Source: FitBit Fitness Tracker Data')


## Daily steps vs sleep quality
final_df %>% 
  select(TotalMinutesAsleep, TotalSteps) %>% 
  mutate(sleep_quality = ifelse(TotalMinutesAsleep <= 420, 'Less than 7h',
                                ifelse(TotalMinutesAsleep <= 540, '7h to 9h', 
                                       'More than 9h'))) %>% 
  mutate(active_level = ifelse(TotalSteps >= 15000,'More than 15,000 steps',
                               ifelse(TotalSteps >= 10000,'10,000 to 14,999 steps',
                                      ifelse(TotalSteps >= 5000, '5,000 to 9,999 steps',
                                             'Less than 4,999 steps')))) %>% 
  select(-c(TotalMinutesAsleep, TotalSteps)) %>% 
  drop_na() %>% 
  group_by(sleep_quality, active_level) %>% 
  summarise(counts = n()) %>% 
  mutate(active_level = factor(active_level, 
                               levels = c('Less than 4,999 steps',
                                          '5,000 to 9,999 steps',
                                          '10,000 to 14,999 steps',
                                          'More than 15,000 steps'))) %>% 
  mutate(sleep_quality = factor(sleep_quality, 
                                levels = c('Less than 7h','7h to 9h',
                                           'More than 9h'))) %>% 
  ggplot(aes(x = sleep_quality, 
             y = counts, 
             fill = sleep_quality)) +
  geom_bar(stat = "identity") +
  custom_theme() +
  scale_fill_manual(values=c("tan1", "#66CC99", "lightcoral")) +
  facet_wrap(~active_level, nrow = 1) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(colour = 'black', size = 8)) +
  theme(strip.background = element_rect(fill = "beige", color = 'black'))+
  labs(
    title = "Daily steps vs sleep quality",
    x = "Sleep quality",
    y = "Count",
    caption = 'Data Source: FitBit Fitness Tracker Data')



## Daily steps vs weight (kg)
final_df %>% 
  select(Id, WeightKg, TotalSteps) %>% 
  group_by(Id) %>% 
  summarise_all(list(~mean(., na.rm=TRUE))) %>% 
  drop_na() %>% 
  mutate(Id = factor(Id)) %>% 
  ggplot(aes(WeightKg, TotalSteps, fill = Id)) +
  geom_point(aes(color = Id, size = WeightKg), alpha = 0.5) +
  scale_size(range = c(5, 20)) +
  custom_theme() +
  theme(legend.position = "none") +
  labs(
    title = "Daily steps vs weight (kg)",
    x = "Kilograms",
    y = "Total Steps",
    caption = 'Data Source: FitBit Fitness Tracker Data')

View(final_df)

