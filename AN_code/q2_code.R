# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
# Team: Group 3: Arunima Roy, Jennifer Grosz, Mikayla Davis, Glen Lewis, Andrew Nalundasan
# Date: 3/17/2021
# Data Translation Challenge
# Q2: How has retail fared relative to other industries?
# Data Extraction

# load libraries
library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
# library(tidylog)
library(estimatr)
library(srvyr) # install.packages("srvyr")
library(zoo) # dates library with yearmo
options(scipen=999) # remove scientific notation
library(data.table)
library(ggplot2)
library(stats)

# read in data files
retail <- read_csv('../AN_raw_data/cps_00002.csv.gz')

# load industry name / code file
inddf <- read_csv("../AN_raw_data/indnames.csv")

####### Working with Data ##########
# rename merge-key column
retail <- retail %>% rename(ind = IND)
# join retail with inddf:
retail_with_ind <- left_join(retail, inddf, by = "ind") %>%
  # filter out NAs
  filter(indname != "NA")


retail_vs_ind_v4 <- retail_with_ind %>% 
  # create year_month variable
  unite(year_month, c("YEAR", "MONTH"), sep = "-", remove = FALSE)  %>%
  # convert to a yearmon data type
  mutate(year_month = as.yearmon(year_month),
         # create covid variable
         covid = case_when(
           year_month < as.yearmon("2020-4") ~ 0,
           year_month > as.yearmon("2020-2") ~ 1)) %>%
  # drop march 2020 from data set and filter out data from before 2019
  filter(year_month != as.yearmon("2020-3"), YEAR > 2018) %>%
  # create time variable
  mutate(time = cumsum(c(1,as.numeric(diff(year_month))!=0)),
         # create dummy employed variable where 1 = employed and 0 = unemployed
         employed = case_when(
           EMPSTAT %in% c(10, 12) ~ 1, 
           EMPSTAT %in% c(20, 21, 22) ~ 0),
         # create categorical industry variable
         industry = case_when(
           ind %between% c(4670,5790) ~ "Retail",
           ind %between% c(8560, 8690) ~ "Arts_Ent_Rec_Accom_Food",
           ind == 770 ~ "Construction",
           ind %between% c(6870, 7190)  ~ "FINC_Ins_Real_Estate",
           ind %between% c(9370, 9590) ~ "Public_Admin"),
         # factor industry variable
         industry = factor(industry)) %>%
  # filter out rows from data set with NAs in industry column
  filter(industry != "NA") %>% 
  # reorder the levels of industry factor for Retail to be on top
  mutate(industry = factor(industry, levels = rev(levels(industry)))) %>%
  filter(WTFINL != "NA", employed != "NA")

######## add population weight to survey data ##############
survey <- 
  as_survey(retail_vs_ind_v4, weights = c(WTFINL)) 

# calculate employment rate for all industries
industry_rate <- survey %>%
  group_by(YEAR, MONTH, industry) %>%
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>%
  pivot_wider(names_from = industry, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

# merge data sets:
merged_df <- left_join(retail_vs_ind_v4, industry_rate, by = c("YEAR", "MONTH"))

# convert year_month to data data type for plot
merged_df <- merged_df %>% 
  mutate(year_month = as.Date(year_month))

# we finally have the data tidied and in the correct format that we need. time to do analysis:
# industry reg:
industry_reg_z <- lm(employed ~ industry*covid + covid*time, merged_df)

export_summs(industry_reg_z, digits = 5, lush = TRUE)


# Plot industries against each other
industry_plot <- ggplot(merged_df, aes(year_month)) +
  
  geom_line(aes(
    y = employment_rate_Arts_Ent_Rec_Accom_Food, color = "Arts-Entertainment-Recreation-Food")) +
  geom_ribbon(aes(ymax = employment_rate_upp_Arts_Ent_Rec_Accom_Food,
                  ymin = employment_rate_low_Arts_Ent_Rec_Accom_Food), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_Construction, color = "Construction")) +
  geom_ribbon(aes(ymax = employment_rate_upp_Construction,
                  ymin = employment_rate_low_Construction), alpha=0.2) +
  
  geom_line(aes(
    y = employment_rate_FINC_Ins_Real_Estate, color = "Finance-Insurance-Real Estate")) +
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
  
  labs(title = "Employment Rates for Industries of Interest",
       x = "Date",
       y = "Employment Rate")  +
  
  theme(legend.title = element_blank()) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  
  theme(axis.text.x=element_text(angle=60, hjust=1))

# look at the plot
industry_plot
