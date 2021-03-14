# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Team: Group 3: Arunima Roy, Jennifer Grosz, Mikayla Davis, Glen Lewis, Andrew Nalundasan
# Date: 3/6/2021
# Data Translation Challenge
# Q2: How has retail fared relative to other industries?
# Data Extraction

# load libraries
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
library(data.table)
library(ggplot2)

# read in data files
retail <- read_csv('../AN_raw_data/cps_00002.csv.gz')

# load industry name / code file
inddf <- read_csv("../AN_raw_data/indnames.csv")

# look at the data
# write.csv(retail, '../AN_raw_data/retail.csv', row.names = FALSE)
# write.csv(employment, 'AN_raw_data/employment.csv', row.names = FALSE)

####### Working with Data ##########

# join retail with inddf:
retail <- retail %>% rename(ind = IND)
retail_with_ind <- left_join(retail, inddf, by = "ind") %>%
  filter(indname != "NA")


# mutate variables for industries of interest
# mutate 'employed' variable
retail_vs_ind <- retail_with_ind %>% 
  mutate(Retail = ind %between% c(4670,5790),
         Arts_Ent_Rec_Accom_Food = ind %between% c(8560, 8690),
         Construction = ind == 770,
         FINC_Ins_Real_Estate = ind %between% c(6870, 7190),
         Public_Admin = ind %between% c(9370, 9590),
         employed = case_when(
           EMPSTAT %in% c(01, 10, 12) ~ 1,
           EMPSTAT %in% c(20, 21, 22) ~ 0)) 

# unite YEAR + MONTH to create 'yrmo' and mutate
# mutate 'covid' variable for pre-covid and post-covid
retail_vs_ind <- retail_vs_ind %>% 
  unite("yrmo", c("YEAR", "MONTH"), sep = '', remove=FALSE) %>% 
  mutate(yrmo = as.numeric(yrmo),
         covid = case_when(
           yrmo < 20204 ~ 0,
           yrmo > 20202 ~ 1),
         time = cumsum(c(1, as.numeric(diff(yrmo)) != 0))
  ) %>% 
  filter(YEAR >= 2019)

# mutate industry and define different industries of interest
retail_vs_ind_v2 <- retail_vs_ind %>% 
  mutate(
    industry = case_when(
      Retail == 1 ~ "Retail",
      Arts_Ent_Rec_Accom_Food == 1 ~ "Arts_Ent_Rec_Accom_Food",
      Construction == 1 ~ "Construction",
      FINC_Ins_Real_Estate == 1 ~ "FINC_Ins_Real_Estate",
      Public_Admin == 1 ~ "Public_Admin")
  )

# filter out NA's from dataset and make industry a factor variable
retail_vs_ind_v2 <- retail_vs_ind_v2 %>% 
  filter(industry != "NA") %>% 
  mutate(industry = factor(industry))

vtable(retail_vs_ind_v2)
# ^^ interact these Industry variables *covid
# ^^ interact covid*time

# regression models: 
m1 <- lm(employed ~ Retail, data = retail_vs_ind_v2)
m2 <- lm(employed ~ Retail + industry*covid, data = retail_vs_ind_v2)

export_summs(m1)


######## add weight to survey data 
retail_vs_ind_v2 <- retail_vs_ind_v2 %>% 
  filter(WTFINL != "NA") %>% 
  filter(employed != "NA")


survey <- 
  as_survey(retail_vs_ind_v2, weights = c(WTFINL)) 


## 
industry_rate <- survey %>%
  group_by(YEAR, MONTH, industry) %>%
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>%
  pivot_wider(names_from = industry, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

# merge dataset:
merged_df <- left_join(retail_vs_ind_v2, industry_rate, by = c("YEAR", "MONTH"))

merged_df <- merged_df %>% 
  unite(year_month, c("YEAR", "MONTH"), sep = "-")  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))

vtable(merged_df)

# Plot
industry_plot <- ggplot(merged_df, aes(year_month)) +
  
  geom_line(aes(
    y = employment_rate_Arts_Ent_Rec_Accom_Food, color = "Arts and stuff")) +
  geom_ribbon(aes(ymax = employment_rate_upp_Arts_Ent_Rec_Accom_Food,
                  ymin = employment_rate_low_Arts_Ent_Rec_Accom_Food), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Construction, color = "Construction")) +
  geom_ribbon(aes(ymax = employment_rate_upp_Construction,
                  ymin = employment_rate_low_Construction), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_FINC_Ins_Real_Estate, color = "Finance Insurance Real Estate")) +
  geom_ribbon(aes(ymax = employment_rate_upp_FINC_Ins_Real_Estate,
                  ymin = employment_rate_low_FINC_Ins_Real_Estate), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Public_Admin, color = "Public Administration")) +
  geom_ribbon(aes(ymax = employment_rate_upp_Public_Admin,
                  ymin = employment_rate_low_Public_Admin), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Retail, color = "Retail")) +
  geom_ribbon(aes(ymax = employment_rate_upp_Retail,
                  ymin = employment_rate_low_Retail), alpha=0.2) +
  
  labs(title = "Employment Rates for Different Industries",
       x = "Date",
       y = "Employment Rate")  +
  
  theme(legend.title = element_blank()) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  
  theme(axis.text.x=element_text(angle=60, hjust=1))

print(industry_plot)


# Q1:Regression
## layoff ~ independent variables + other variables
retail_rate <- survey %>% 
  group_by(YEAR, MONTH) %>% 
  summarize(retail_rate = survey_mean(employed))







# Regression: 
# WHYUNEMP ~ Retail + Industry A + Industry B + Industry C + YEAR + e
# I need to tune up this model
# What other models would be of value?

# pull out Industries -- Retail vs. others
# difference between mine and JG - she puts "other industry" into "Other" and I break it up to different ones
retail_vs_sector <- retail_select %>% 
  mutate(Retail = Industry %between% c(4670,5790),
         Arts_Ent_Rec_Accom_Food = Industry %between% c(8560, 8690),
         Construction = Industry == 770,
         FINC_Ins_Real_Estate = Industry %between% c(6870, 7190),
         Public_Admin = Industry %between% c(9370, 9590)
  )

vtable(retail_vs_sector)


m1 <- lm(WHYUNEMP ~ Retail + Arts_Ent_Rec_Accom_Food + Construction + FINC_Ins_Real_Estate +
           Public_Admin, data = retail_vs_sector)

m2 <- lm(Retail ~ YEAR, data = retail_vs_sector)

## need to interact COVID with each industry


export_summs(m1, m2)





# look at variables and take note of Class
vtable(retail)

# select necessary variables
retail_select <- retail %>% select("YEAR", "MONTH", "CPSID", "STATEFIP", "Occupation", "Industry_1990 basis", "Industry", "Industry Last Year")
vtable(retail_select)

# group by Industry
# retail_group_by <- retail_select %>% group_by(Industry)

# plot the data to look at raw relationship
#density <- ggplot(retail_select, aes(x = Industry)) + 
#  geom_density() + 
#  labs(x = 'Industry', title = 'Distribution of Industry across Years')
# density


#ggplot(data = retail_group_by) +
#  geom_histogram(mapping = aes(x = Industry), binwidth = 1000) +
#  xlab('Industry') +
#  ggtitle('Distribution of Industry across Years')

# scatterplot by year
# retail_select %>% 
#  group_by(YEAR, Industry) %>% 
#  ggplot(mapping = aes(x = YEAR, y = Industry, color = Industry)) +
#  geom_point() +
#  my_theme +
#  labs(title = "Distribution of Industries across Years",
#       subtitle = "Industries and Years",
#       x = "Year",
#       y = "Industry") +
#  scale_color_manual(values = c("winter" = "#d6dee1", "spring" = "#d6dee1",
#                                "summer" = "#002060", "fall" = "#d6dee1")) +
#  theme(axis.text.x = element_blank(),
#        axis.ticks.x = element_blank())



##### Data Viz ###########
# my_theme <- theme(panel.border = element_blank(),
#                  panel.background = element_blank(),
#                  axis.line = element_line(color = "gray"),
#                  axis.ticks = element_line(color = "gray"),
#                  plot.title = element_text(color = "#808080", size = 16, face = "bold"),
#                  plot.subtitle = element_text(color = "#808080", size = 14, face = "plain"),
#                  axis.text.x = element_text(color = "#808080"),
#                  axis.text.y = element_text(color = "#808080"),
#                  axis.title = element_text(color = "#808080", size = 10, face = "plain"),
#                  legend.title = element_text(color = "#808080", size = 12, face = "plain"),
#                  legend.text = element_text(color = "#808080", size = 10, face = "plain"),
#                  plot.caption = element_text(color = "#808080", size = 10, face = "plain")
#)



## Mikayla Viz ##


#sum of unemployed
sum_unemp <- wip_emp_data %>%
  count(survey_dates) %>%
  filter(survey_dates >= "2019-11-01")

plot_total <- ggplot(data = sum_unemp, aes(survey_dates, n)) +
  geom_bar(stat = "identity") + 
  plot_total

sum_by_why <- wip_emp_data %>%
  count(survey_dates, whyunemp) %>%
  filter(survey_dates >= "2019-11-01")

plot_by_why <- sum_by_why %>%
  ggplot(aes(x = survey_dates, y = n, fill = factor(whyunemp)))+
  scale_fill_discrete(name = "Reason",
                      labels=c("Laid Off", 
                               "Temporary Work Concluded", "Loss of Work")) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position="bottom") + 
  xlab("Month") +
  ylab("Number of People")

plot_by_why