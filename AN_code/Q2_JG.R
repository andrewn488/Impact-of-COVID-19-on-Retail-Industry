library(tidyverse)
library(vtable)
library(jtools)
library(haven)
library(tidylog)
options(scipen=999)

# Load data set
df <- read_dta("../AN_raw_data/cps_00010.dta/cps_00010.dta")

# load industry name / code file
inddf <- read_csv("../AN_raw_data/indnames.csv")

# join to get industry names in data
joined_df <- left_join(df, inddf, by = "ind") %>%
  filter(indname != "NA")

# redo for indly (I ended up dropping this variable, but if you wanted to keep it in there, this is one way to do it)
inddf2 <- read_csv("../AN_raw_data/indnames.csv") %>%
  rename(industryly = "indname", indly = "ind") 
# join again to get last years industry name in dataset
joined_df <- left_join(joined_df, inddf2, by = "indly")


### AN - this is getting to comparison of Retail vs. Other Industry (lines 37-38)
df_a <- joined_df %>%
  # cpsid = 0 is what I was having an issue with in my df, so I just dropped them. 
  # I'll be checking with NHK tomorrow to see if thats an okay thing to do
  # labforce 2 - yes in laborforce
  filter(cpsid != 0, labforce == 2)  %>%
  # I didnt include hhrhid in my dataset, so I just pulled it from the cspid variable
  # On a second thought... this isnt necessary at all. I think you can ignore everything
  # i've included about creating a person ID.
  mutate(hhid = substr(cpsid, 7, 12),
         # dummy variable for retail v other industries
         retail = case_when(
           ind %in% c(4670:5790) ~ 1,
           ind %in% c(1:4669, 5790:9890) ~ 0),
         # dummy variable for employed v unemployed
         employed = case_when(
           empstat %in% c(01,10,12) ~ 1,
           empstat %in% c(20, 21, 22) ~0)) 

# this is so I can have the unique person id in dataset
# this wasn't necessary after all
df_b <- df_a %>%
  mutate(pernum = as.numeric(pernum)) %>%
  unite("id", c("hhid", "pernum"), sep = "-")

# filter data
df_b <- df_b %>%
  select(id, year, month, employed, retail, indname, ind, wkstat, 
         inctot, covidtelew, covidunaw, covidlook) %>%
  # check out the dates in your data set, I noticed this with mine for Q3, but saw the
  # same thing here. data collected pre 11/2019 is not monthly data its annual and 
  # collected in march, after 11/2019 the data is monthly. I'll also be asking NHK about
  # this today when we chat
  unite("yrmo", c("year", "month"), sep = '', remove=FALSE) %>%
  mutate(yrmo = as.numeric(yrmo)) 
#%>%
 # filter(yrmo > 20193)


df_c <- df_b %>%
  # I probably should have made a dummy for unemployed instead of employed in the first place
  mutate(unemployed = case_when(
    employed == 0 ~ 1,
    employed == 1 ~ 0),
    # I looked at the COVID variables, and they dont offer much, but if you narrow down
    # to just 11/2019 onward, they might be more useful
    # dummy for telework
    teleworkCOVID = case_when(
      covidtelew == 1 ~ 0,
      covidtelew == 2 ~ 1),
    # dummy for unable to work
    unabletoworkCOVID = case_when(
      covidunaw == 1 ~ 0,
      covidunaw == 2 ~ 1),
    # dummy for unable to look for work - I dropped this, it was all NA's
    unabletolookCOVID = case_when(
      covidlook == 1 ~ 0,
      covidlook == 2 ~ 1)) %>%
  # group by year month and industry (this is why I no longer thing you need a person identifier)
  # if you're looking at industry, the actual people dont really matter as much as knowing which 
  # industry they're working in
  group_by(year, month, ind) %>%
  # generate monthly variables for headcounts of people in these variables per industry
  mutate(numUnemp = sum(unemployed),
         numTelework = sum(teleworkCOVID),
         numUnableWork = sum(unabletoworkCOVID),
         # I'll verify inctot is the right variable to use for this because I have that
         # question for Q3's data set
         meanIncome = mean(inctot)
         # if you did a monthly time of 11/2019 and later data then It might be useful 
         # to see what the total income is per industry per month, but that would really
         # depend on how many people are working in each ind each month or else you may 
         # get some skewed data here. I didnt look at what this looked like after I created
         # this variable
         industryIncome_monthly = sum(inctot)) %>%
  ungroup()


# this is about as far as I got with my own dataset for Q3 today, but I hope you find 
# something i've included in here to be useful!











