
############### Load libraries
library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(tidylog)
library(estimatr)
library(srvyr) # install.packages("srvyr")
library(car)
options(scipen=999) # remove scientific notation

############### Load Data
df <- read_dta("01_DataQ3/cps_00012.dta") 

vtable(df)

df <- df %>%
  filter(empstat != 1, labforce == 2) %>% # excluding military employment and people not in laborforce
  mutate(
    female = case_when(
      sex == 1 ~ 0, # 1 - Male
      sex == 2 ~ 1), # 2 - Female
    married = case_when(
      marst == 1 ~ 1, # 1 - Married, Spouse present
      marst %in% c(2,3,4,5,6) ~ 0),# 2 - Married Spouse absent, 3 - separated, 4- Divorced, 5 - widowed, 6 - never married/single
    employed = case_when(
      empstat %in% c(10, 12) ~ 1, # 10 - At Work, 12 - Has Job not work since last week
      empstat %in% c(20, 21, 22) ~ 0),# 20 - Unemployed, 21 - unemp experienced worker, 22- unemp new worker
    unemployed = case_when(
      employed == 1 ~ 0, # opposite of employed
      employed == 0 ~ 1),# opposite of employed
    highestEduc = case_when(
      educ99 == 1 ~ "no school completed",
      educ99 == 4 ~ "1st - 4th grade",
      educ99 == 5 ~ "5th - 8th grade",
      educ99 == 6 ~ "9th grade",
      educ99 == 7 ~ "10th grade",
      educ99 == 8 ~ "11th grade",
      educ99 == 9 ~ "12th grade, no diploma",
      educ99 == 10 ~ "High School graduate or GED",
      educ99 == 11 ~ "Some college no degree",
      educ99 == 13 ~ "Associate degree occupational program",
      educ99 == 14 ~ "Associate degree academic program",
      educ99 == 15 ~ "Bachelor's degree",
      educ99 == 16 ~ "Master's degree",
      educ99 == 17 ~ "Professional degree",
      educ99 == 18 ~ "Doctorate degree"),
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
    childrenHH = case_when(
      nchild != 0 ~ 1,
      nchild == 0 ~ 0)) %>% 
  unite("yrmo", c(year, month), sep = '', remove = FALSE) %>%
  mutate(
    yrmo = as.numeric(yrmo),
    covid = case_when(
      yrmo < 20204 ~ 0,
      yrmo > 20202 ~ 1)) %>%
  filter(yrmo != 20203, year > 2018) %>%
  mutate(
    time = cumsum(c(1,as.numeric(diff(yrmo))!=0))) %>%
  select(year, month, wtfinl, age, sex, racialCategories, childrenHH, female, married, #asecwt, - when I ran data with this included it was all NA values for this data set, if we use INCTOT variable, we'll need this weight
         employed, unemployed, highestEduc, covid, time) 

######## weight data 
survey <- 
  as_survey(df, weights = c(wtfinl)) 

######## Employment Rate full data
employment_monthly <- survey %>% 
  group_by(year, month) %>% 
  summarize(employment_rate = survey_mean(employed))

######## Unemployment Rate full data
unemployment_monthly <- survey %>% 
  group_by(year, month) %>% 
  summarize(unemployment_rate = survey_mean(unemployed))

# join data
monthly_rates <- left_join(employment_monthly, unemployment_monthly, by = c("year", "month"))

#vtable(monthly_rates)

###### merge monthly rates back into data set
df_b <- left_join(df, monthly_rates, by = c("year", "month"))

ggplot(df_b, aes(x = covid*time, y = employment_rate), color = factor(female)) + 
  geom_point() + 
  xlab("Monthly Employment Rate") +
  ylab("Employment Rate") +
  geom_vline(aes(xintercept=14.5), color = 'blue', linetype='dashed') +
  theme(axis.text.x = element_blank())

############# REGRESSIONS

### Female Regression Results 
femalereg <- lm_robust(employment ~ female*covid + covid*time, survey)
export_summs(femalereg, digits = 9, robust = TRUE)

# Regression results + female vs male employment:
ggplot(data=df_b, 
       aes(x=covid, y = employment_rate, 
           group= female, 
           color=factor(female))) + 
  geom_line() + 
  xlab("Covid") + 
  ylab("Employment Rate") + 
  scale_color_discrete(name = "Sex", 
                       labels=c("Male", "Female"))

agereg <- lm_robust(employment_rate ~ age*covid + covid*time, survey)
export_summs(agereg, digits = 9, robust = TRUE)

racereg <- lm_robust(employment_rate ~ racialCategories*covid + covid*time, survey)
export_summs(racereg, digits = 9, robust = TRUE)

marriedreg <- lm_robust(employment_rate ~ married*covid + covid*time, survey)
export_summs(marriedreg, digits = 9, robust = TRUE)

educreg <- lm_robust(employment_rate ~ highestEduc*covid + covid*time, survey)
export_summs(educreg, digits = 9, robust = TRUE)

childrenreg <- lm_robust(employment_rate ~ childrenHH*covid + covid*time, survey)
export_summs(childrenreg, digits = 9, robust = TRUE)

####################################################################
############## Employment & unemployment rates per group ###########

############## FEMALE
## females v males monthly employment
female_emp_monthly <- survey %>% 
  group_by(year, month, female) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = female, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## females v males monthly unemployment
female_unemp_monthly <- survey %>% 
  group_by(year, month, female) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = female, values_from = c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
female_rates_monthly <- left_join(female_emp_monthly, female_unemp_monthly,by = c("year", "month"))



############## Married
## married v non-married monthly employment rate
married_emp_monthly <- survey %>% 
  group_by(year, month, married) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = married, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## married v non-married monthly unemployment rate
married_unemp_monthly <- survey %>% 
  group_by(year, month, married) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = married, values_from = c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
married_rates_monthly <- left_join(married_emp_monthly, married_unemp_monthly,by = c("year", "month"))


############## Age
# monthly unemployment rate by age
age_emp_monthly <- survey %>% 
  group_by(year, month, age) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = age, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## monthly unemployment rate by age
age_unemp_monthly <- survey %>% 
  group_by(year, month, age) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = age, values_from = c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
age_rates_monthly <- left_join(age_emp_monthly, age_unemp_monthly,by = c("year", "month"))



############## race
# monthly unemployment rate by racial category
race_emp_monthly <- survey %>% 
  group_by(year, month, racialCategory) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = racialCategory, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## monthly unemployment rate by racial category
race_unemp_monthly <- survey %>% 
  group_by(year, month, racialCategory) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = racialCategory, values_from = c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
race_rates_monthly <- left_join(race_emp_monthly, race_unemp_monthly,by = c("year", "month"))



############## Educ
# monthly unemployment rate by racial category
educ_emp_monthly <- survey %>% 
  group_by(year, month, highestEduc) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = highestEduc, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## monthly unemployment rate by race
educ_unemp_monthly <- survey %>% 
  group_by(year, month, highestEduc) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = highestEduc, values_from=c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
educ_rates_monthly <- left_join(educ_emp_monthly, educ_unemp_monthly, by = c("year", "month"))


############## Children in HH
# monthly unemployment rate children in hh v no children
child_emp_monthly <- survey %>% 
  group_by(year, month, childrenHH) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = childrenHH, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## monthly unemployment rate by children in hh v no children
child_unemp_monthly <- survey %>% 
  group_by(year, month, childrenHH) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = childrenHH, values_from = c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
educ_rates_monthly <- left_join(child_emp_monthly, child_unemp_monthly, by = c("year", "month"))




















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
  filter(year > 2018) %>%
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


# Write final data set to file for regression analysis
#write_csv(df,
         # file = "01_DataQ3/mydata.csv",
         # na = "NA",
        #  append = FALSE,
         # col_names = TRUE,
         # quote_escape = "double",
         # eol = "\n")






























df2 <- read_dta("01_DataQ3/cps_00014.dta") 

df2_a <- df2 %>%
  filter(year > 2018) %>%
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



