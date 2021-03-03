#library build ----

library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)

# data load and review ----

base_emp_data <- read_dta('01_raw_data/cps_00003.dta')

vt(base_emp_data, lush = TRUE)

wip_emp_data <- base_emp_data %>% filter(!(whyunemp == 0)) %>% 
  select('year', 'month', 'serial', 'pernum', 'occ2010', 'whyunemp') %>% 
  filter(occ2010 %in% c(4700, 4720, 4740, 4760)) %>% 
  filter(whyunemp %in% c(1,2,3)) %>% 
  class_conversion(c('occ' , 'why'), as.factor) %>% 
  class_conversion('month', as.numeric)


vt(wip_emp_data, lush = TRUE)

ggplot(data = wip_emp_data, aes(x = whyunemp)) +
  geom_bar () +
  facet_grid(~year)
  
  
            
