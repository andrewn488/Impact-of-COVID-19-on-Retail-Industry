# Author: Andrew Nalundasan
# For: OMSBA 5300, Seattle University
#Team: Group 3: Arunima Roy, Jennifer Grosz, Mikayla Davis, Glen Lewis, Andrew Nalundasan
# Date: 3/6/2021
# Data Translation Challenge
# Q2: How has retail fared relative to other industries?
# Data Extraction

# load libraries
library(tidyverse)
library(haven)
library(jtools)
library(vtable)
library(data.table)

# read in data files
retail <- read_csv('../AN_raw_data/cps_00002.csv.gz')
# employment <- read_dta('AN_raw_data/cps_00004.dta.gz')

# look at the data
# write.csv(retail, '../AN_raw_data/retail.csv', row.names = FALSE)
# write.csv(employment, 'AN_raw_data/employment.csv', row.names = FALSE)

# added 'whyunemp', 'statefip', variables and reduced time to 2017-2021

####### Working with Data ##########

# rename variables to be understandable
retail_select <- retail %>% select(YEAR, STATEFIP, OCC, IND, WHYUNEMP) %>% 
  rename("Occupation" = OCC, "Industry" = IND)

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




