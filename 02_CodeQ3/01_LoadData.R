############### Load libraries
library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(tidylog)
library(estimatr)
library(ggplot2)
library(dplyr)
library(plotly)
library(srvyr) # install.packages("srvyr")
library(zoo) # dates library with yearmo
options(scipen=999) # remove scientific notation


############### Load Data
df <- read_dta("../01_DataQ3/cps_00012.dta") 

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
    ageGeneration = case_when(
      age %in% c(16:22) ~ "GenerationZ",
      age %in% c(23:38) ~ "Millennials",
      age %in% c(39:54) ~ "GenerationX",
      age %in% c(55:73) ~ "BabyBoomers"),
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
      race == 300 ~ "American_Indian_Aleut_Eskimo",
      race == 651 ~ "Asian",
      race == 652 ~ "Hawaiian_Pacific_Islander",
      race %in% c(801:820, 830) ~ "Mixed")) %>%
  unite(year_month, c("year", "month"), sep = "-", remove = FALSE)  %>%
  mutate(year_month = as.yearmon(year_month),
         covid = case_when(
           year_month < as.yearmon("2020-4") ~ 0,
           year_month > as.yearmon("2020-2") ~ 1)) %>%
  filter(year_month != as.yearmon("2020-3"), year > 2018) %>%
  mutate(time = cumsum(c(1,as.numeric(diff(year_month))!=0))) %>%
  select(year, month, year_month, wtfinl, ageGeneration, racialCategories, 
         female, married, employed, educationCategories, covid, time) 

######## add weight to survey data 
survey <- as_survey(df, weights = c(wtfinl)) 

# Check correlation between independent variables
cor(df[c( "covid", "time")])

######################################## Gender Analysis ############################################
#####################################################################################################
### Linear Probability Model: Gender Regression Results 
femalereg <- lm_robust(employed ~ female*covid + covid*time, survey)
female_reg_table <- export_summs(femalereg, digits = 9, robust = TRUE)
female_reg_table

# calculate employment rate male v female  
female_emp_monthly <- survey %>% 
  group_by(year, month, female) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = female, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
# join data
female_df <- left_join(df, female_emp_monthly, by = c("year", "month"))
# rename columns, add date variables
vtable(female_df)
female_df_z <- female_df %>%
  rename(male_employment_rate = 'employment_rate_0',
         female_employment_rate = 'employment_rate_1',
         maleHigh = "employment_rate_upp_0",
         maleLow = "employment_rate_low_0",
         femaleHigh = "employment_rate_upp_1",
         femaleLow = "employment_rate_low_1") %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Plot
emp_women <- ggplot(female_df_z, aes(year_month)) + 
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

emp_women
##################################### Marriage Status Analysis #######################################
######################################################################################################
### Linear Probability Model: Marriage Status Regression Results 
marriedreg <- lm_robust(employed ~ married*covid + covid*time, survey)
married_reg_table <- export_summs(marriedreg, digits = 9, robust = TRUE)
married_reg_table

# calculate employment rate married v non-married
married_emp_monthly <- survey %>% 
  group_by(year, month, married) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = married, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
## join data
married_df <- left_join(df, married_emp_monthly, by = c("year", "month"))
# rename columns
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

# Plot
emp_married <- ggplot(married_df_z, aes(year_month)) + 
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

emp_married
##################################### Age Generation Analysis ########################################
######################################################################################################
### Linear Probability Model:  Age Generation Regression Results 
agereg <- lm_robust(employed ~ ageGeneration*covid + covid*time, survey)
age_reg_table <- export_summs(agereg, digits = 9, robust = TRUE)
age_reg_table

# calculate employment rate age categories
age_emp_monthly <- survey %>% 
  group_by(year, month, ageGeneration) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = ageGeneration, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
# join data
age_df <- left_join(df, age_emp_monthly, by = c("year", "month"))
# convert to years
age_df_z <- age_df %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

# Plot
emp_age <- ggplot(age_df_z, aes(year_month)) + 
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

emp_age
########################################  Education Level Analysis ############################################
###############################################################################################################
### Linear Probability Model:  Education Category Regression Results 
educCreg <- lm_robust(employed ~ educationCategories*covid + covid*time, survey)
educ_reg_table <- export_summs(educCreg, digits = 9, robust = TRUE)
educ_reg_table

# calculate employment rate by education level category
educ_emp_monthly <- survey %>% 
  group_by(year, month, educationCategories) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = educationCategories, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))
# join data
educ_df <- left_join(df, educ_emp_monthly, by = c("year", "month"))
# add dates
educ_df_z <- educ_df %>%
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

#Plot
emp_educ_cat <- ggplot(educ_df_z, aes(year_month)) + 
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

emp_educ_cat
#####################################  Racial Category Analysis ############################################
######################################################################################################
### Linear Probability Model:  Racial Category Regression Results 
racereg <- lm_robust(employed ~ racialCategories*covid + covid*time, survey)
race_reg_table <-export_summs(racereg, digits = 9, robust = TRUE)
race_reg_table

# calculate monthly employment rate for racial categories
race_emp_monthly <- survey %>% 
  group_by(year, month, racialCategories) %>% 
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>% 
  pivot_wider(names_from = racialCategories, values_from = c(employment_rate,employment_rate_low,employment_rate_upp)) %>%
  ungroup()
## join data
race_df <- left_join(df, race_emp_monthly, by = c("year", "month"))
# convert to years 
race_df_z <- race_df %>% 
  unite(year_month, c("year", "month"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month)) 

# Plot
emp_race <- ggplot(race_df_z, aes(year_month)) + 
  geom_line(aes(
    y = employment_rate_White, color = "White")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_White, 
                  ymin = employment_rate_low_White), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Black, color = "Black")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Black, 
                  ymin = employment_rate_low_Black), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_American_Indian_Aleut_Eskimo, color = "American Indian/Aleut/Eskimo")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_American_Indian_Aleut_Eskimo, 
                  ymin = employment_rate_low_American_Indian_Aleut_Eskimo), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Asian, color = "Asian")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Asian, 
                  ymin = employment_rate_low_Asian), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Hawaiian_Pacific_Islander, color = "Hawaiian/Pacific Islander")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Hawaiian_Pacific_Islander, 
                  ymin = employment_rate_low_Hawaiian_Pacific_Islander), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Mixed, color = "Mixed")) + 
  geom_ribbon(aes(ymax = employment_rate_upp_Mixed, 
                  ymin = employment_rate_low_Mixed), alpha=0.2) +
  
  labs(title = "Employment Rates for Racial Groups", 
       x = "Date", 
       y = "Employment Rate")  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

emp_race

#################### race pie chart
# Filter dataset to Jan 2020
race_df_jan_20 <- race_df_z %>% 
  filter(year_month >= "2020-01-01", year_month <= "2020-01-31") %>% 
  filter(employed == 1) %>% 
  count(racialCategories)

# Filter dataset to Jan 2021
race_df_jan_21 <- race_df_z %>% 
  filter(year_month >= "2021-01-01", year_month <= "2021-01-31") %>% 
  filter(employed == 1) %>% 
  count(racialCategories)

# Pie Chart for Jan 2020
fig1 <- plot_ly(race_df_jan_20, labels = ~racialCategories, values = ~n, type = 'pie')
fig1 <- fig1 %>% 
  layout(title = "Employed by Race in January 2020",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig1
# Pie Chart for Jan 2021
fig2 <- plot_ly(race_df_jan_21, labels = ~racialCategories, values = ~n, type = 'pie')
fig2 <- fig2 %>% 
  layout(title = "Employed by Race in January 2021",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig2




