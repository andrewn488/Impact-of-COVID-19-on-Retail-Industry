#library build ----

library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(estimatr)

# data load ----

base_emp_data <- read_dta('01_raw_data/cps_00007.dta')
base_closure_data <- read.csv('01_raw_data/state_lockdown_data.csv')
base_state_covid_history_data <- read.csv('01_raw_data/all-states-history.csv')
base_google_trends_data <- read.csv('01_raw_data/multiTimeline.csv')

# wip = work in progress = this is the dataframe that is manipulated 

# following code establishes a function to convert variable class()

class_conversion <- function(df, x, y){
  'str_prefix' = x
  'new_class' = y
  df %>% mutate(across(starts_with(str_prefix), new_class))
}

# employment data wrangling ----

# following code begins by filtering out 'whyunemp == 0' which is used to 
# catalog those in the military or government service.  
# it then selects the desired variables. It begins by filtering the data
# keeping only data for people who report a retail occupations (47xx), 
# then filters 'whyunemp == 1,2,3' which correspond to unemployment due to 
# layoffs, ending of temporary work, and general lose of work.  
# It converts the variables 'occ2010', 'covidunaw' and 'whyunemp' 
# from class(numeric) to class(factor).

wip_emp_data <- base_emp_data %>% filter(!(whyunemp == 0)) %>% 
  filter(occ2010 %in% c(4700:5790)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('month', 'durunemp', 'statefip'), as.numeric) %>% 
  class_conversion(c('occ', 'covidunaw', 'whyunemp'), as.factor) %>% 
  select(-'serial')

# following code tidies up the data by converting the factor levels in 'whyunemp' 
# and 'covidunaw' to individual variables with the observation being the 
# total number of 'whyunemp', 'covidunaw' reported for the month.  It then 
# removes the base columns. 

wip_emp_data <- wip_emp_data %>% group_by(year, month, statefip) %>% 
  mutate(layoff = sum(whyunemp == 1), other_type_loss = sum(whyunemp == 2), 
  temp_job_end = sum(whyunemp == 3), covidunaw = sum(covidunaw == 2)) %>% 
  mutate(covid_impact_no = sum(covidunaw == 1), 
  covid_impact_yes = sum(covidunaw == 2)) %>% 
  select(-'whyunemp', -'covidunaw', -'occ2010', -'empstat') %>% 
  summarise(sum(layoff), sum(other_type_loss), sum(temp_job_end), 
            sum(covid_impact_yes), sum(covid_impact_no))

# following code concatenates year, month variables then adds a day piece, 
# to create a class(date) variable.

wip_emp_data <- wip_emp_data %>% 
  mutate(survey_dates = ymd(paste0(year, '-', month, '-01')))

# state stay at home data wrangling ----

# converts base_closure_data variable 'lck_dwn_st' from class(character) to class(date)

wip_state_closures <- base_closure_data %>% 
  mutate(lck_dwn_st = mdy(lck_dwn_st))

# covid-19 state history wrangling ----

# following code builds a key to join the state covid-19 history data with the 
# employment and stay at home order (SAH) data

state_name_key <- read.csv('01_raw_data/state_names.csv')

state_name_key <- state_name_key %>% select(-'Abbrev') %>% 
  rename(state_code = Code)

wip_state_covid_history_data <- base_state_covid_history_data %>% 
  rename(state_code = state) %>% mutate(date = mdy(date))

wip_state_covid_history_data <- wip_state_covid_history_data %>% 
  left_join(state_name_key, 'state_code')

# following code wrangles the state covid history, beginning with selecting the 
# desired variables and removes NA's associated with what appears to be an an error

wip_state_covid_history_data <- wip_state_covid_history_data %>% 
  rename(state = State) %>% 
  select('date', 'state', 'positive', 'positiveIncrease','hospitalizedIncrease') %>% 
  na.omit()

# following code tidies the covid state history data starting with grouping by 
# state and date then filtering for all data from April 1 2020. The code then 
# splits and creates a month and year columns to group by then summarizing the 
# variables to produce a single month observation for each state. It then builds
# a new date column to be used in the join.

wip_state_covid_history_data <- wip_state_covid_history_data %>% 
  group_by(date, state) %>% 
  summarise(positive, positiveIncrease, hospitalizedIncrease) %>%
  mutate(month = month(date), year = year(date)) %>% 
  group_by(state, month, year) %>% 
  summarise(sum(positive), sum(positiveIncrease), sum(hospitalizedIncrease)) %>% 
  mutate(date = ymd(paste0(year, '-', month, '-01')))

# google trends wrangling ----

wip_google_trends <- base_google_trends_data %>% 
  mutate(Week = mdy(Week)) %>% mutate(Week = ymd(Week)) %>% 
  rename(date = Week)

# base dataframe build----

base_emp_closure_covid_df <- wip_emp_data %>% 
  rename(date = survey_dates) %>% 
  left_join(wip_state_closures, 'statefip') %>% 
  left_join(wip_state_covid_history_data, by = c('state', 'date')) %>% 
  rename(total_cases = 'sum(positive)', 
         daily_increase_pos_test = 'sum(positiveIncrease)', 
         daily_increase_hosp = 'sum(hospitalizedIncrease)', 
         layoff = 'sum(layoff)', other_type_loss = 'sum(other_type_loss)',
         temp_job_end = 'sum(temp_job_end)', 
         covid_impact_yes = 'sum(covid_impact_yes)', 
         covid_impact_no = 'sum(covid_impact_no)')



  

            
