#library build ----

library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)

# data load and initial wrangling ----

base_emp_data <- read_dta('01_raw_data/cps_00004.dta')
base_closure_data <- read.csv('02_derived_data/state_lockdown_data.csv')

vt(base_emp_data, lush = TRUE)
vt(base_closure_data, lush = TRUE)

# filters out 'whyunemp == 0' which is used to catalogs those in the military or 
# government service.  Selects the desired variables. It further filters the data
# keeping only the data for people in retail occupations (47xx), 'whyunemp == 1,2,3' 
# which correspond to unemployment due to layoffs, ending of temporary work, and 
# lose of work.  It converts the variables 'occ2010' and 'whyunemp' from numeric 
# to class(factor).

# wip = work in progress = this is the dataframe that is manipulated 

wip_emp_data <- base_emp_data %>% filter(!(whyunemp == 0)) %>% 
  select('year', 'month', 'serial', 'statefip', 'pernum', 'occ2010', 'whyunemp') %>% 
  filter(occ2010 %in% c(4700, 4720, 4740, 4760)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('occ' , 'why'), as.factor) %>% 
  class_conversion('month', as.numeric)

# concatenates year, month variables then adds a day piece, to create a class(date)
# variable.

wip_emp_data <- wip_emp_data %>% 
  mutate(survey_dates = ymd(paste0(year, '-', month, '-01')))

# converts base_closure_data variable 'lck_dwn_st' from class(charater) to class(date)

wip_state_closures <- base_closure_data %>% 
  mutate(lck_dwn_st = mdy(lck_dwn_st))

# builds a complete dataframe with a left join, based on the variable 'statefip'

wip_emp_closure_data <- wip_emp_data %>% left_join(wip_state_closures, 'statefip')

# reorders the variables 

wip_emp_closure_data <- wip_emp_data[c(8,1,2,3,4,6,7,5)]

vt(wip_emp_data, lush = TRUE)


  
  
            
