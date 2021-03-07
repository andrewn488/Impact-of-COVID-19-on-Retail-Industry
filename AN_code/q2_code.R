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
retail <- retail %>% rename("State (FIPS code)" = EMPSTAT, "Occupation" = OCC, "Industry_1990 basis" = IND1990, "Industry" = IND, 'Reason for unemployment' = WHYUNEMP, "Industry Last Year" = INDLY, "Income from worker's' compensation" = INCWKCOM, "Job Type" = JTYPE, "Worked remotely for pay due to pandemic" = COVIDTELEW, "Unable to work due to pandemic" = COVIDUNAW, "Received pay for hours not worked due to pandemic" = COVIDPAID, "Prevented from looking for work due to pandemic" = COVIDLOOK)

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








