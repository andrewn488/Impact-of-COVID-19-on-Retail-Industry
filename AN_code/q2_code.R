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
library(stats)

# read in data files
retail <- read_csv('../AN_raw_data/cps_00002.csv.gz')

# load industry name / code file
inddf <- read_csv("../AN_raw_data/indnames.csv")

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

# reorder the levels of industry factor for Retail to be on top
retail_vs_ind_v3 <- retail_vs_ind_v2 %>% 
  filter(industry != "NA") %>% 
  mutate(industry = factor(industry, levels = rev(levels(industry))))

######## add weight to survey data ##############
retail_vs_ind_v4 <- retail_vs_ind_v3 %>% 
  filter(WTFINL != "NA") %>% 
  filter(employed != "NA")

survey <- 
  as_survey(retail_vs_ind_v4, weights = c(WTFINL)) 


# make rate for all industries
industry_rate <- survey %>%
  group_by(YEAR, MONTH, industry) %>%
  summarize(employment_rate = survey_mean(employed, vartype = "ci")) %>%
  pivot_wider(names_from = industry, values_from = c(employment_rate,employment_rate_low,employment_rate_upp))

# merge datasets:
merged_df <- left_join(retail_vs_ind_v4, industry_rate, by = c("YEAR", "MONTH"))

# create year_month variable and set type to date
merged_df <- merged_df %>% 
  unite(year_month, c("YEAR", "MONTH"), sep = "-", remove = FALSE)  %>%
  mutate(year_month = (Date = as.yearmon(year_month))) %>%
  mutate(year_month = as.Date(year_month))


# Employment Rate 
employment_rate <- survey %>% 
  group_by(YEAR, MONTH) %>% 
  summarize(employment_rate = survey_mean(employed))

# merge employment rate into df
merged_df <- left_join(merged_df, employment_rate, by = c("YEAR", "MONTH")) 


########### Industry Analysis ################
# retail reg
retail_reg <- lm(employment_rate ~ Retail*covid + covid*time, merged_df)

# arts and stuff reg
Arts_Ent_Rec_Accom_Food_reg <- lm(employment_rate ~ Arts_Ent_Rec_Accom_Food*covid + covid*time, merged_df)

# construction reg
construction_reg <- lm(employment_rate ~ Construction*covid + covid*time, merged_df)

# FINC_Ins_Real_Estate reg
FINC_Ins_Real_Estate_reg <- lm(employment_rate ~ FINC_Ins_Real_Estate*covid + covid*time, merged_df)

# Public_Admin reg
Public_Admin_reg <- lm(employment_rate ~ Public_Admin*covid + covid*time, merged_df)

# look at the regressions
export_summs(retail_reg, Arts_Ent_Rec_Accom_Food_reg, construction_reg,
             FINC_Ins_Real_Estate_reg, Public_Admin_reg, digits = 5,
             lush = TRUE)

# industry reg:
#  <- lm_robust(employed ~ industry*covid + covid*time, merged_df)
industry_reg_z <- lm(employment_rate ~ industry*covid + covid*time, merged_df)

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
  
  labs(title = "Employment Rates for Different Industries",
       x = "Date",
       y = "Employment Rate")  +
  
  theme(legend.title = element_blank()) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %b") +
  
  theme(axis.text.x=element_text(angle=60, hjust=1))

# look at the plot
industry_plot



##### working code - demeaned approach ######
# demeaned industries
# merged_df_dm <- merged_df %>% group_by(industry) %>% 
#  mutate(retail_dm = Retail - mean(Retail),
#         Arts_Ent_Rec_Accom_Food_dm = Arts_Ent_Rec_Accom_Food - mean(Arts_Ent_Rec_Accom_Food),
#         Constructin_dm = Construction - mean(Construction),
#         FINC_Ins_Real_Estate_dm = FINC_Ins_Real_Estate - mean(FINC_Ins_Real_Estate),
#         Public_Admin_dm = Public_Admin - mean(Public_Admin))

# fe_dm <- lm(employed ~ retail_dm + Arts_Ent_Rec_Accom_Food_dm + Constructin_dm +
#              FINC_Ins_Real_Estate_dm + Public_Admin_dm, merged_df_dm)

# LSDV: 
# fe_lsdv <- lm(employed ~ Retail + covid + industry, merged_df)
# linearHypothesis(fe_lsdv, matchCoefs(fe_lsdv, 'industry'))
