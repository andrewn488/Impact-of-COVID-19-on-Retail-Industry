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