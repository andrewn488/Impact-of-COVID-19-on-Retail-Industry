############### Load libraries
library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(tidylog)
library(estimatr)
library(srvyr) # install.packages("srvyr")
library(zoo) # dates library with yearmo
options(scipen=999) # remove scientific notation

############### Load Data
df <- read_dta("01_DataQ3/cps_00012.dta") 

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
    ageGeneration = case_when(
      age %in% c(16:22) ~ "GenerationZ",
      age %in% c(23:38) ~ "Millennials",
      age %in% c(39:54) ~ "GenerationX",
      age %in% c(55:73) ~ "BabyBoomers"),
    highestEduc = case_when(
      educ99 == 1 ~ "no_school_completed",
      educ99 == 4 ~ "1st_4th grade",
      educ99 == 5 ~ "5th_8th grade",
      educ99 == 6 ~ "9th_grade",
      educ99 == 7 ~ "10th_grade",
      educ99 == 8 ~ "11th_grade",
      educ99 == 9 ~ "12th_grade_no_diploma",
      educ99 == 10 ~ "High_School_graduate_or_GED",
      educ99 == 11 ~ "Some_college_no_degree",
      educ99 == 13 ~ "Associate_degree_occupational_program",
      educ99 == 14 ~ "Associate_degree_academic_program",
      educ99 == 15 ~ "Bachelors_degree",
      educ99 == 16 ~ "Masters_degree",
      educ99 == 17 ~ "Professional_degree",
      educ99 == 18 ~ "Doctorate_degree"),
    educationCategories = case_when(
      educ99 %in% c(1:9) ~ 'Below_HS_Graduate',
      educ99 %in% c(10, 11) ~ 'HighSchoolGraduate',
      educ99 %in% c(13,14) ~ 'AssociateDegree',
      educ99 == 15 ~ 'BachelorDegree',
      educ99 == 16 ~ 'MasterDegree',
      educ99 == 17 ~ 'ProfessionalDegree',
      educ99 == 18 ~ 'DoctorateDegree'),
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
  select(year, month, wtfinl, ageGeneration, age, sex, racialCategories, childrenHH, 
         female, married, employed, unemployed, highestEduc, educationCategories, covid, time) 

######## add weight to survey data 
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

# join employment rates into one dataset
monthly_rates <- left_join(employment_monthly, unemployment_monthly, by = c("year", "month"))

###### merge monthly rates back into data set
#df_b <- left_join(df, monthly_rates, by = c("year", "month"))

######################################## Gender Analysis ############################################
######################################################################################################
### Linear Probability Model: Gender Regression Results 
femalereg <- lm_robust(employed ~ female*covid + covid*time, survey)
export_summs(femalereg, digits = 9, robust = TRUE)

### Plot Employment Rates: females v males monthly employment
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
female_df <- left_join(df, female_rates_monthly, by = c("year", "month"))

vtable(female_df)
female_df_z <- female_df %>%
  rename(male_employment_rate = 'employment_rate_0',
         female_employment_rate = 'employment_rate_1',
         male_unemployment_rate = 'unemployment_rate_0',
         female_unemployment_rate = 'unemployment_rate_1',
         maleHigh = "employment_rate_upp_0",
         maleLow = "employment_rate_low_0",
         femaleHigh = "employment_rate_upp_1",
         femaleLow = "employment_rate_low_1",
         umaleLow = "unemployment_rate_low_0",
         ufemaleLow = "unemployment_rate_low_1",
         umaleHigh = "unemployment_rate_upp_0",
         ufemaleHigh = "unemployment_rate_upp_1") %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Employment rate men V women
ggplot(female_df_z, aes(year_month)) + 
  geom_line(aes(
    y = male_employment_rate, color = "men")) + 
  geom_ribbon(aes(ymax = maleHigh, ymin = maleLow), alpha=0.2) +
  geom_line(aes(
    y = female_employment_rate, color = "women")) + 
  geom_ribbon(aes(ymax = femaleHigh, ymin = femaleLow), alpha=0.2) +
  labs(title = "Employment Rates For Men and Women", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))


# unemployment rates
ggplot(female_df_z, aes(year_month)) + 
  geom_line(aes(
    y = male_unemployment_rate, color = "men")) + 
  geom_ribbon(aes(ymax = umaleHigh, ymin = umaleLow), alpha=0.2) +
  geom_line(aes(
    y = female_unemployment_rate, color = "women")) + 
  geom_ribbon(aes(ymax = ufemaleHigh, ymin = ufemaleLow), alpha=0.2) +
  labs(title = "Unemployment Rates for Men and Women", 
       x = "Date", 
       y = "Unemployment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

##################################### Marriage Status Analysis ########################################
######################################################################################################
### Linear Probability Model: Marriage Status Regression Results 
marriedreg <- lm_robust(employed ~ married*covid + covid*time, survey)
export_summs(marriedreg, digits = 9, robust = TRUE)

### Plot Employment Rates: married v non-married monthly employment
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
married_df <- left_join(df, married_rates_monthly, by = c("year", "month"))

married_df_z <- married_df %>%
  rename(not_employment_rate = 'employment_rate_0',
         not_eHigh = "employment_rate_upp_0",
         not_eLow = "employment_rate_low_0",
         married_employment_rate = 'employment_rate_1',
         married_eHigh = "employment_rate_upp_1",
         married_eLow = "employment_rate_low_1") %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Employment rate Married V Not Married
ggplot(married_df_z, aes(year_month)) + 
  geom_line(aes(
    y = not_employment_rate, color = "Not Married")) + 
  geom_ribbon(aes(ymax = not_eHigh, ymin = not_eLow), alpha=0.2) +
  geom_line(aes(
    y = married_employment_rate, color = "Married")) + 
  geom_ribbon(aes(ymax = married_eHigh, ymin = married_eLow), alpha=0.2) +
  labs(title = "Employment Rates For Married And Not Married People", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

##################################### Age Generation Analysis ########################################
######################################################################################################
### Linear Probability Model:  Age Generation Regression Results 
agereg <- lm_robust(employed ~ ageGeneration*covid + covid*time, survey)
export_summs(agereg, digits = 9, robust = TRUE)

### Plot Employment Rates: 
# Employment rates by age
age_emp_monthly <- survey %>% 
  group_by(year, month, ageGeneration) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = ageGeneration, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

## Unemployment rate by age
age_unemp_monthly <- survey %>% 
  group_by(year, month, ageGeneration) %>% 
  summarize(unemployment_rate = survey_mean(unemployed, vartype = "ci")) %>% 
  pivot_wider(names_from = ageGeneration, values_from = c(unemployment_rate,unemployment_rate_low,unemployment_rate_upp))

## join data
age_rates_monthly <- left_join(age_emp_monthly, age_unemp_monthly,by = c("year", "month"))
age_df <- left_join(df, age_rates_monthly, by = c("year", "month"))

# convert to years
age_df_z <- age_df %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

# Plot
ggplot(age_df_z, aes(year_month)) + 
  geom_line(aes(
    y = employment_rate_BabyBoomers, color = "Baby Boomers")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_BabyBoomers, 
                  ymin = employment_rate_low_BabyBoomers), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_GenerationX, color = "Generation X")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_GenerationX, 
                  ymin = employment_rate_low_GenerationX), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_GenerationZ, color = "Generation Z")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_GenerationZ, 
                  ymin = employment_rate_low_GenerationZ), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_low_Millennials, color = "Millennials")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Millennials, 
                  ymin = employment_rate_low_Millennials), alpha=0.2) +
  
  labs(title = "Employment Rates for Age Generations", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

################################# People in Households with Children Analysis ########################################
######################################################################################################
### Linear Probability Model:  Children living in Household Regression Results 
childrenreg <- lm_robust(employed ~ childrenHH*covid + covid*time, survey)
export_summs(childrenreg, digits = 9, robust = TRUE)

### Plot Employment Rates: 
# People with children in hh v no children Employment
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
child_rates_monthly <- left_join(child_emp_monthly, child_unemp_monthly, by = c("year", "month"))
child_df <- left_join(df, child_rates_monthly, by = c("year", "month"))

child_df_z <- child_df %>%
  rename(no_employment_rate = 'employment_rate_0',
         no_High = "employment_rate_upp_0",
         no_Low = "employment_rate_low_0",
         child_employment_rate = 'employment_rate_1',
         child_High = "employment_rate_upp_1",
         child_eLow = "employment_rate_low_1") %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))


# Plot 
ggplot(child_df_z, aes(year_month)) + 
  geom_line(aes(
    y = no_employment_rate, color = "No Children in HH")) + 
  geom_ribbon(aes(ymax = no_High, ymin = no_Low), alpha=0.2) +
  geom_line(aes(
    y = child_employment_rate, color = "Children in HH")) + 
  geom_ribbon(aes(ymax = child_High, ymin = child_eLow), alpha=0.2) +
  labs(title = "Employment Rates For People living In Households With Children And Without Children", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))



########################################  Education Level Analysis ############################################
###############################################################################################################
### Linear Probability Model:  Highest Education Regression Results 
educreg <- lm_robust(employed ~ highestEduc*covid + covid*time, survey)
export_summs(educreg, digits = 9, robust = TRUE)

### Linear Probability Model:  Education Category Regression Results 
educCreg <- lm_robust(employed ~ educationCategories*covid + covid*time, survey)
export_summs(educCreg, digits = 9, robust = TRUE)


# monthly employment rate by education level category
educ_emp_monthly <- survey %>% 
  group_by(year, month, educationCategories) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = educationCategories, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

educ_df <- left_join(df, educ_emp_monthly, by = c("year", "month"))
vtable(educ_df)

educ_df_z <- educ_df %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Plot
ggplot(educ_df_z, aes(year_month)) + 
  geom_line(aes(
    y = employment_rate_AssociateDegree, color = "Associate Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_AssociateDegree, 
                  ymin = employment_rate_low_AssociateDegree), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_BachelorDegree, color = "Bachelor's Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_BachelorDegree, 
                  ymin = employment_rate_low_BachelorDegree), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Below_HS_Graduate, color = "Below High School Graduate")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Below_HS_Graduate, 
                  ymin = employment_rate_low_Below_HS_Graduate), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_DoctorateDegree, color = "Doctorate Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_DoctorateDegree, 
                  ymin = employment_rate_low_DoctorateDegree), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_HighSchoolGraduate, color = "High School Graduate")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_HighSchoolGraduate, 
                  ymin = employment_rate_low_HighSchoolGraduate), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_MasterDegree, color = "Master's Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_MasterDegree, 
                  ymin = employment_rate_low_MasterDegree), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_ProfessionalDegree, color = "Professional Degree")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_ProfessionalDegree, 
                  ymin = employment_rate_low_ProfessionalDegree), alpha=0.2) +
  
  labs(title = "Employment Rates for Education Levels", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))


#####################################  Racial Category Analysis ############################################
######################################################################################################
### Linear Probability Model:  Racial Category Regression Results 
racereg <- lm_robust(employed ~ racialCategories*covid + covid*time, survey)
export_summs(racereg, digits = 9, robust = TRUE)


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



