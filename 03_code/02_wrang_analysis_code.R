# the effects of covid on the retail industry's health ----

# initial H0 = COVID did not impact retail industry health and measured by unemployment.
# initial HA = COVID related measures - specifically stay at home orders (SAH) impacted
# the retail industries health.

# following code  builds a df with the variables of interest: state, layoffs, 
# closure dates. Year and month variables are retained for potential group_by()
# function. 

state_closure_layoff_df <- wip_emp_data %>% 
  select('state', 'month', 'year', 'layoff') %>% 
  left_join(wip_state_closures, 'state')

# model of SAH order start dates and layoffs.  

lm_state_closure_layoff_df <- lm_robust(layoff ~ lck_dwn_st + factor(state), 
                                        data = state_closure_layoff_df)  

export_summs(lm_state_closure_layoff_df) # no relationship between layoffs and SAH 
# order start dates.

state_covid_cases_layoff_df <- wip_emp_data %>% 
  select('state', 'month', 'year', 'layoff')

state_covid_cases_layoff_df <-  base_state_covid_history_data %>% 
  select('date', 'state', 'positive') %>% 
  mutate(date = mdy(date)) %>% 
  mutate(date = ymd(date)) %>% 
  left_join(state_covid_cases_layoff_df, 'state') 




# follow-up visualizations of layoff, covid-19 cases, and google search trends for covid ----

# visualizations: the following code does some specific data wrangling
# and produces some initial data visualizations. The wrangling includes 
# standardizing of all the variables then merged into a single dataframe. 

wip_layoff_google_trends <- wip_google_trends %>% 
  filter(!(Covid...United.States.) == '<1') %>% 
  filter(date >= as.Date('2019-12-01')) %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month) %>% 
  class_conversion('Covid...United.States.', as.numeric) %>% 
  summarise(monthly_trend_mean = mean(Covid...United.States.), 
            trend_sd = sd(Covid...United.States.)) %>% 
  mutate(yearly_mean = mean(monthly_trend_mean), 
         monthly_trend_sd = sd(monthly_trend_mean), 
         std_month_score = (monthly_trend_mean - yearly_mean)/monthly_trend_sd,
         date = ymd(paste0(year, '-', month, '-01')))
         
wip_emp_mod_1 <- exploration_base_employment_df %>% 
  rename(date = 'survey_dates') %>% 
  filter(date >= as.Date('2019-12-01')) %>% 
  select('date', 'layoff', 'year', 'month') %>% 
  group_by(date) %>% 
  summarise(monthly_av_layoff = mean(layoff)) %>% 
  mutate(yearly_av_layoff = mean(monthly_av_layoff),
         monthly_sd = sd(monthly_av_layoff),
         monthly_std_layoff = (monthly_av_layoff - yearly_av_layoff)/monthly_sd)

wip_state_covid_history_mod_1 <- base_state_covid_history_data %>% 
  select('date', 'state', 'positive') %>% 
  filter(date >= as.Date('2019-12-01')) %>% 
  mutate(year = year(date), month = month(date)) %>% 
  na.omit() %>% 
  group_by(year, month) %>% 
  summarise(national_monthly_positive = sum(positive)) %>% 
  mutate(mean_positive = mean(national_monthly_positive), 
         positive_sd = sd(national_monthly_positive), 
         std_monthly_positive = (national_monthly_positive - mean_positive)/positive_sd, 
         date = ymd(paste0(year, '-', month, '-01')))

base_layoff_gtrends_covid_cases <- wip_emp_mod_1 %>% 
  left_join(wip_layoff_google_trends, 'date') %>% 
  left_join(wip_state_covid_history_mod_1)
  
ggplot(base_layoff_gtrends_covid_cases) + 
  geom_smooth( aes(x =date, y = monthly_std_layoff, colour = "Layoffs"), se = FALSE) + 
  geom_smooth(aes(x = date, y = std_month_score, colour = 'Google COVID \nSearch Trends'), se = FALSE) +
  geom_smooth(aes(x = date, y = std_monthly_positive, colour = 'Total COVID \nCases'), se = FALSE) +
  xlim(as.Date('2019-12-01'), as.Date('2021-01-01')) +
  geom_vline(xintercept = as.Date("2020-03-18"), color = 'black') +
  geom_vline(xintercept = as.Date("2020-12-18"), color = 'orange') +
  geom_vline(xintercept = as.Date("2020-12-11"), color = 'orange') + 
  labs(colour = "Legend, \nStandardized Values") +
  ggtitle('Google COVID Search Trends v. Retail Layoffs v. Total COVID Cases') +
  xlab('Date') + 
  ylab('Standard Deviations') +
  geom_text(aes(x=as.Date("2020-03-01"), label="\n1st SAH Order", y=-1.75), colour="black", 
            angle=90, text=element_text(size=4)) +
  geom_text(aes(x=as.Date("2020-12-01"), label="Vaccine Authorizations", y=-1.5), colour="orange", 
            angle=90, text=element_text(size=4))

# following code analyzes potential relationship between google search trends, 
# covid cases and layoffs ----

lm_gtrend_ccases_layoffs <-  lm_robust( monthly_std_layoff ~ std_month_score +
                             std_monthly_positive, data = base_layoff_gtrends_covid_cases)

export_summs(lm_gtrend_ccases_layoffs, 
             model.names = 'Layoff v. Covid Search Trends v. Total Cases')

effect_plot(lm_gtrend_ccases_layoffs, std_month_score, plot.points = TRUE, 
            x.label = 'Timeline', 
            y.label =  'Reported Layoff per Month (Standardized)',
            point.color = 'blue2', 
            main.title = 'Layoff v. Covid Search Trends v. Total Cases')

# following code will use a fixed effects model 

wip_emp_data_mod_2 <- wip_emp_data %>% 
  rename(date = 'survey_dates') %>% 
  filter(date >= as.Date('2020-01-01')) %>% 
  select('date', 'state', 'layoff', 'year', 'month') %>% 
  group_by(state, year, month) %>% 
  summarise(layoff = sum(layoff)) %>% 
  mutate(date = ymd(paste0(year, '-', month, '-01')))
  
base_state_layoff_gtrends <- wip_emp_data_mod_2 %>% 
  full_join(wip_layoff_google_trends, 'date')

lm_state_layoff_gtrends <-  lm_robust(layoff ~ std_month_score + factor(state), 
                                       data = base_state_layoff_gtrends)

export_summs(lm_state_layoff_gtrends, 
             model.names = 'TBD')



wip_state_covid_history_mod_2 <- base_state_covid_history_data %>% 
  select('date', 'state', 'positive') %>% 
  mutate(date = mdy(date)) %>% 
  mutate(date = ymd(date)) %>%
  mutate(year = year(date), month = month(date)) %>% 
  filter(!(state == 'AS'), date >= as.Date('2020-01-01')) %>% 
  group_by(state, year, month) %>% 
  summarise(total_cases = sum(positive)) %>% 
  mutate(date = ymd(paste0(year, '-', month, '-01')))

base_state_layoff_gtrends_case <- wip_emp_data_mod_2 %>% 
  left_join(wip_layoff_google_trends, 'date') %>% 
  left_join(wip_state_covid_history_mod_2, 'date')

lm_base_state_layoff_gtrends_case <-  lm_robust(layoff ~ std_month_score + total_cases +
                                                factor(state.x), 
                                                data = base_state_layoff_gtrends_case)
export_summs(lm_base_state_layoff_gtrends_case, 
             model.names = 'TBD')
 
# DiD wrangling and analysis ----
# dummy variable build - creating a dummy variable for states that implemented 
# a stay-at-home order (1) and those states that did not (0)

wip_emp_closure_did <- base_emp_closure_data %>% 
  mutate(treated = case_when((!is.na(lck_dwn_st) ~ 1), (is.na(lck_dwn_st) ~ 0))) %>% 
  class_conversion('treated', as.factor)
    
# creating a dummy variable for the pre-treatment times and post-treatment times.
# could have used simpler code but wanted to keep dummy variables in same format.

wip_emp_closure_did <- wip_emp_closure_did %>% 
  mutate(treatment_after = case_when((survey_dates >= as.Date('2020-03-18') ~ 1), 
  (survey_dates <= as.Date('2020-03-18') ~ 0))) %>% 
  class_conversion('treatment_after', as.factor) 

# national DiD modeling----

# the following model looks at the variable 'reason for unemployment' (whyunemp) 
# against treated and treatment_after variables for all states in the survey to 
# identify differences between those with stay-at-home orders and those without.
# With the hypothesis stating that stay-at-home orders resulting from COVID-19 
# is what affected the health of the retail industry. The following attempts to 
# assess if the stay-at-home orders did affect unemployment as a sign of industry health.  

national_data <- wip_emp_closure_did %>% 
  group_by(state, survey_dates) %>% 
  summarise(lck_dwn_st, layoff, other_type_loss, temp_job_end, covidunaw, treated, 
  treatment_after) %>% 
  distinct()

# plots the rate un those unemployed due to being laid off against survey dates.
# the red line is the date of first lock downs.

ggplot(national_data, aes(x=survey_dates, y=layoff)) + 
  geom_smooth(se = FALSE) + 
  geom_smooth(aes(x = survey_dates, y = covidunaw), color = 'black', se = FALSE) +
  geom_vline(xintercept = as.Date("2020-03-18"), color = 'red') +
  xlab('Survey Dates') +
  ylab('Reported Unemployment Rates') +
  ggtitle('National Layoffs and COVID Related Layoff verse Survey Dates') 


lm_national <- lm_robust(layoff ~ treated + treatment_after + treated:treatment_after, 
                       data = national_data)

export_summs(lm_national, model.names = 'National DiD Layoff Model')

effect_plot(lm_national, treated, plot.points = TRUE, 
            x.label = 'SAH Orders Inplace or Not', 
            y.label =  'Reported Layoff per Month',
            point.color = 'blue2', 
            main.title = "DiD National Model Effects Plot between Stay-at-Home Order States")

# because the national model compares states with different population sizes, 
# densities and economies that could impact the modeling, the next model filters the 
# data set to a group of midwest states (Iowa, Nebraska, Minnesota, Wisconsin, 
# Illinois, Missouri, Kansas, Colorado, Wyoming) which either had or did not have 
# stay-at-home orders (Iowa and Nebraska). This model also includes 
# the variable that specifically asks if unemployment is COVID-19 related. 

# ASSUMPTION: the proximity of the control and treatment states leads to similar 
# populations densities, economies, etc. 

regional_data <- wip_emp_closure_did %>% 
  filter(state %in% c('Minnesota', 'Wisconsin', 'Illinois', 'Missouri', 'Kansas',
  'Colorado', 'Wyoming','Iowa', 'Nebraska')) %>% 
  group_by(state, survey_dates) %>% 
  summarise(lck_dwn_st, layoff, other_type_loss, temp_job_end, covidunaw, treated,
  treatment_after) %>% 
  distinct()

ggplot(regional_data, aes(x=survey_dates, y=layoff)) + 
  geom_smooth(se = FALSE) + 
  geom_smooth(aes(x = survey_dates, y = covidunaw), color = 'black', se = FALSE) +
  geom_vline(xintercept = as.Date("2020-03-18"), color = 'red') +
  xlab('Survey Dates') +
  ylab('Reported Unemployment Rates') +
  ggtitle('Regional Layoffs and COVID Related Unemployment verse Survey Dates') 

lm_regional <- lm_robust(layoff ~ treated + treatment_after + treated:treatment_after, 
                            data = regional_data)

export_summs(lm_national, lm_regional, 
             model.names = c('National DiD Layoff', 'Regional DiD Layoff'))                      

effect_plot(lm_regional, treated, plot.points = TRUE, 
            x.label = 'SAH Orders Inplace or Not', 
            y.label =  'Regional Reported Layoff Rates per Month',
            point.color = 'blue2', 
            main.title = "DiD Regional Model between Stay-at-Home Order States")

# the following models takes the regional data and looks at the relationship 
# between report of covid related unemployment and whether the state has a SAH order.

lm_regional_covid <- lm(covidunaw ~ treated, data = regional_data)

export_summs(lm_regional_covid)                      

effect_plot(lm_regional_covid, treated, plot.points = TRUE)

# ASSUMTPION: As the virus began to spread across the country and local 
# authorities started implementing individual restrictions - first State 
# Emergency Declaration was 9 March - retail unemployment 
# (and thus the health of the retail sector) may have started to go downhill prior to 
# stay at home orders. 

# Since we are looking for changes in rates of unemployment
# specifically (layoff) before and after stay-at-home orders filtering duration 
# employed to 4 weeks and less should allow us to capture initial layoffs that
# started occurring before stay-at-home orders as a result of individual actions.

