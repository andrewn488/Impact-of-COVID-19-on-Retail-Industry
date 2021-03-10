#library build ----

library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)

# data load ----

base_emp_data <- read_dta('01_raw_data/cps_00007.dta')
base_closure_data <- read.csv('02_derived_data/state_lockdown_data.csv')

# base dataframe wrangling ----

# filters out 'whyunemp == 0' which is used to catalogs those in the military or 
# government service.  Selects the desired variables. It further filters the data
# keeping only the data for people in retail occupations (47xx), 'whyunemp == 1,2,3' 
# which correspond to unemployment due to layoffs, ending of temporary work, and 
# lose of work.  It converts the variables 'occ2010' and 'whyunemp' from numeric 
# to class(factor).

# wip = work in progress = this is the dataframe that is manipulated 

class_conversion <- function(df, x, y){
  'str_prefix' = x
  'new_class' = y
  df %>% mutate(across(starts_with(str_prefix), new_class))
}

wip_emp_data <- base_emp_data %>% filter(!(whyunemp == 0)) %>% 
  filter(occ2010 %in% c(4700, 4720, 4740, 4760)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('month', 'durunemp'), as.numeric) %>% 
  class_conversion(c('occ', 'covidunaw'), as.factor)

# concatenates year, month variables then adds a day piece, to create a class(date)
# variable.

wip_emp_data <- wip_emp_data %>% 
  mutate(survey_dates = ymd(paste0(year, '-', month, '-01'))) %>% 
  select(-'year', -'month')

# following code tiddies up the data by converting the factor levels in whyunemp
# to individual variables 

wip_emp_data <- wip_emp_data %>% group_by(survey_dates, statefip) %>% 
  mutate(layoff = sum(whyunemp == 1), other_type_loss = sum(whyunemp == 2), 
  temp_job_end = sum(whyunemp == 3), covidunaw = sum(covidunaw == 2)) %>% 
  select(-'whyunemp')

# converts base_closure_data variable 'lck_dwn_st' from class(character) to class(date)

wip_state_closures <- base_closure_data %>% 
  mutate(lck_dwn_st = mdy(lck_dwn_st))

# builds a complete dataframe with a left join, based on the variable 'statefip'

base_emp_closure_data <- wip_emp_data %>% left_join(wip_state_closures, 'statefip')


  
  
            
