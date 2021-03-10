library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(tidylog)
library(estimatr)
options(scipen=999)

df <- read_dta("01_DataQ3/cps_00012.dta") 

df <- df %>%
  # filter out unmatched march basic data, NA's in income and NIU values for income
  filter(year > 2018, labforce == 2) %>%
  # select necessary variables
  select(cpsidp, year, month, age, sex, race, marst, empstat, educ99, nchild) 

df <- df %>%
  unite("yrmo", c(year, month), sep = '', remove = FALSE) %>%
  filter(yrmo != 20203) %>%
  # create dummy variable to denote working v not working
  mutate(yrmo = as.numeric(yrmo),
         employed = case_when(
           # 01 - Armed Forces, 10 - At Work, 12 - Has Job not work since last week
           empstat %in% c(01, 10, 12) ~ 1,
           # 20 - Unemployed, 21 - unemp experienced worker, 22- unemp new worker
           empstat %in% c(20, 21, 22) ~ 0),
         female = case_when(
           # 1 - Male, 2 - Female
           sex == 1 ~ 0,
           sex == 2 ~ 1),
         male = case_when(
           # 1 - Male, 2 - Female
           sex == 1 ~ 1,
           sex == 2 ~ 0),
         married = case_when(
           # 1 - Married, Spouse present
           marst == 1 ~ 1,
           # 2 - Married Spouse absent, 3 - separated, 4- Divorced, 5 - widowed, 6 - never married/single
           marst %in% c(2,3,4,5,6) ~ 0),
         highestEduc = case_when(
           educ99 == 1 ~ 0, #"no school completed",
           educ99 == 4 ~ 1, #"1st - 4th grade",
           educ99 == 5 ~ 2, #"5th - 8th grade",
           educ99 == 6 ~ 3, #"9th grade",
           educ99 == 7 ~ 4, #"10th grade",
           educ99 == 8 ~ 5, #"11th grade",
           educ99 == 9 ~ 6, #"12th grade, no diploma",
           educ99 == 10 ~ 7, #"High School graduate or GED",
           educ99 == 11 ~ 8, #"Some college no degree",
           educ99 == 13 ~ 9, #"Associate degree occupational program",
           educ99 == 14 ~ 10, #"Associate degree academic program",
           educ99 == 15 ~ 11, # "Bachelor's degree",
           educ99 == 16 ~ 12, #"Master's degree",
           educ99 == 17 ~ 13, #"Professional degree",
           educ99 == 18 ~ 14), #"Doctorate degree"),
         racialCategories = case_when(
           race == 100 ~ "White",
           race == 200 ~ "Black",
           race == 300 ~ "American Indian/Aleut/Eskimo",
           race == 651 ~ "Asian",
           race == 652 ~ "Hawaiian/Pacific Islander",
           race == 801 ~ "White-Black",
           race == 802 ~ "white - American Indian",
           race == 803 ~ "White-Asian",
           race == 804 ~ "White-Hawaiian/Pacific Islander",
           race == 805 ~ "Black-American Indian",
           race == 806 ~ "Black-Asian",
           race == 807 ~ "Black-Hawaiian/Pacific Islander",
           race == 808 ~ "American Indian-Asian",
           race == 809 ~ "Asian-Hawaiian/Pacific Islander",
           race == 810 ~ "White-Black-American Indian",
           race == 811 ~ "White-Black-Asian",
           race == 812 ~ "White-American Indian-Asian",
           race == 813 ~ "White-Asian-Hawaiian Pacific Islander",
           race == 814 ~ "White-Black-American Indian-Asian",
           race == 815 ~ "American Indian - Hawaiian/Pacific Islander",
           race == 816 ~ "White-Black- Hawaiian/Pacific Islander",
           race == 817 ~ "White-American Indian-Hawaiian/Pacific Islander",
           race == 818 ~ "Black-American Indian- Asian",
           race == 819 ~ "White-American Indian-Asian-Hawaiian/Pacific Islander",
           race == 820 ~ "Two or three races, unspecified",
           race == 830 ~ "Four or five races, unspecified"),
         covid = case_when(
           yrmo < 20204 ~ 0,
           yrmo > 20202 ~ 1),
         time = cumsum(c(1,as.numeric(diff(yrmo))!=0))) %>%
  filter(employed == 1, yrmo > 20191) %>%
  select(-cpsidp, -yrmo, -sex, -race, - marst, -empstat, -educ99)

vtable(df)

# Write final data set to file for regression analysis
write_csv(df,
          file = "01_DataQ3/mydata.csv",
          na = "NA",
          append = FALSE,
          col_names = TRUE,
          quote_escape = "double",
          eol = "\n")






























df2 <- read_dta("01_DataQ3/cps_00014.dta") 

df2_a <- df2 %>%
  filter(year > 2014) %>%
  # create unique identifier
  mutate(pid = substr(cpsidp, 7, 14))  %>%
  unite("monthlyid", c(year, month, pid), sep = '-', remove = FALSE)





df3 <-read_dta("01_DataQ3/cps_00015.dta") 

df3_a <- df3 %>%
  filter(year > 2014) %>%
  # create unique identifier
  mutate(pid = substr(cpsidp, 7, 14))  %>%
  unite("monthlyid", c(year, month, pid), sep = '-', remove = FALSE)

##### Duplicates Check ######
# Check data set for duplicates on Key (id)
check_dupes(df3_a, 'monthlyid') # 0 - No Duplicates
dups <- df3_a %>%
  filter(monthlyid %in% unique(.[["monthlyid"]][duplicated(.[["monthlyid"]])]))


# delete duplicated monthly ID's
df3_b <- df3_a %>%
  filter(!(monthlyid %in% dups$monthlyid))

check_dupes(df3_b, 'monthlyid') 

income <- df3_b %>%
  select(monthlyid, pid, year, month, inctot, incwage, hourwage, paidhour, earnweek, wksworkorg)

paid_hrly <- income %>%
  filter(paidhour == 2, hourwage != 999.88) %>%
  select(-inctot, -incwage, -paidhour) %>%
  mutate(monthlyInc = earnweek*4)%>%
  select(monthlyid, monthlyInc)



