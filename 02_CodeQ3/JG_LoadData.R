library(tidyverse)
library(jtools)
library(vtable)
library(haven)
library(lubridate)
library(tidylog)

# Function to check for duplicates 
check_dupes <- function(data, vars) {
  data %>% 
    select(vars) %>%
    duplicated() %>%
    max()
}


# read data file (this is the data I sent you, I'll send you the new one)
demdf <- read_dta("01_DataQ3/cps_00008.dta") 

# New data set
newdf <-  read_dta("01_DataQ3/cps_00009.dta") 
# look at the data
vtable(newdf)
# write.csv(demographics,"01_DataQ3/cps_00008.csv", row.names = FALSE)


newdf_a <- newdf %>%
  # filter out unmatched march basic data, anything pre November 2019
  # and include only people who are in the labor force
  filter(cpsidp != 0, year > 2018, labforce == 2) %>%
  # select necessary variables
  select(year, month, cpsidp, age, #pernum,
         sex, race, marst, empstat, labforce) %>%
  # create dummy variable to denote working v not working
  mutate(working = case_when(
    # 01 - Armed Forces, 10 - At Work, 12 - Has Job not work since last week
    empstat %in% c(01, 10, 12) ~ 1,
    # 20 - Unemployed, 21 - unemp experienced worker, 22- unemp new worker
    empstat %in% c(20, 21, 22) ~ 0))

# create count of people in labor force working per month per year
dem_working_count <- newdf_a %>%
  filter(working == 1) %>%
  group_by(year, month) %>%
  mutate(WorkingCount = n()) %>%
  ungroup()

# create count of people in labor force not working per month per year
# not sure we need this, might not rejoin to data set
dem_notworking_count <- newdf_a %>%
  filter(working == 0) %>%
  group_by(year, month) %>%
  mutate(notWorkingCount = n()) %>%
  ungroup()


dem_working <- dem_working_count %>% 
  # to plot by year and month as one variable, this could be used to 
  # represent a time unit
  unite(date, c(year, month), sep = "-", remove = FALSE) %>%
  mutate(ex = case_when(
    # 1 - Male, 2 - Female
    sex == 1 ~ 1,
    sex == 2 ~ 0),
    married = case_when(
      # 1 - Married, Spouse present
      marst == 1 ~ 1,
      # 2 - Married Spouse absent, 3 - separated, 4- Divorced, 5 - widowed, 6 - never married/single
      marst %in% c(2,3,4,5,6) ~ 0)) %>%
  # drop variables
  select(-marst, -empstat, -labforce, -working) %>%
  # I can explain this - the monthly data was CPS only, the other years' data was for March of every year only, not monthly data set
  filter(date != "2019-3")
vtable(dem_working)

#######################################################
# get educ, nchildren, income data

demdf <- demdf %>%
  filter(cpsidp != 0)
# look at the data
vtable(demdf)
# write.csv(demdf,"01_DataQ3/cps_00008.csv", row.names = FALSE)

# Educ and nchild variables weren't in new data set 
demdf_a <- demdf %>%
  select(cpsidp, educ, nchild, inctot, incwage, hourwage, 
         paidhour) 

#### need to filter out 999999999 from inctot! and go through educ, 
# nchild, incwage, hourwage, paidhour variables/check codes

df <- left_join(dem_working, demdf_a, by = "cpsidp")

# Not sure what this looks like yet, just fyi. It made my computer 
# angry and I had to restart R when I tried running it, there's too many 
# observations in the data set that are repeats and can be narrowed down
# BUT this general graph is what I'm thinking we could do answer 
# "what has changed about whose working" in terms of volume in 
# the workforce working over the months 
# since Covid first came to the US 
ggplot(data=df, aes(x=date, y=WorkingCount)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept="2020-1"), linetype='dashed') #+
#theme(axis.text.x=element_blank())







