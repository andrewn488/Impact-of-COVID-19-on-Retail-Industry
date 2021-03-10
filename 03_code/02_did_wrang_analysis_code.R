# DiD wrangling and analysis

# dummy variable build ----

# creating a dummy variable for states that implemented a stay-at-home order (1) and 
# those states that did not (0)

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
  ggtitle('National Layoffs and COVID Related Unemployment verse Survey Dates') 


lm_national <- lm(layoff ~ treated + treatment_after + treated:treatment_after, 
                       data = national_data)

export_summs(lm_national, model.names = 'National DiD Layoff Model')

effect_plot(lm_national, treatment_after, plot.points = TRUE, 
            x.label = 'Survey Dates', 
            y.label =  'Reported Layoff Rates per Month',
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

lm_regional <- lm(layoff ~ treated + treatment_after + treated:treatment_after, 
                            data = regional_data)

export_summs(lm_national, lm_regional, 
             model.names = c('National DiD Layoff', 'Regional DiD Layoff'))                      

effect_plot(lm_national, treatment_after, plot.points = TRUE, 
            x.label = 'Survey Dates', 
            y.label =  'Regional Reported Layoff Rates per Month',
            point.color = 'blue2', 
            main.title = "DiD Regional Model between Stay-at-Home Order States")

# ASSUMTPION: As the virus began to spread across the country and local 
# authorities started implementing individual restrictions - first State 
# Emergency Declaration was 9 March - retail unemployment 
# (and thus the health of the retail sector) started to go downhill prior to 
# stay at home orders. Since we are looking for changes in rates of unemployment
# specifically (layoff) before and after stay-at-home orders filtering duration 
# employed to 4 weeks and less should allow us to capture initial layoffs that
# started occurring before stay-at-home orders as a result of individual actions.

