# Histogram of rebate received vs. home assessed value

# Where will the breaks be in assessed value to create bins
breaks_val <- c(-Inf, 5e4, 1e5, 1.25e5, 1.5e5, 1.75e5, 2e5, 2.25e5, 2.5e5, 2.75e5, 3e5, 3.5e5, 4e5, 5e5, 7.5e5, Inf)

# Average rebate received vs. assessed value, including participants and non-participants
hd <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            total_rebate = mean(total_paid_inc)) %>% 
  replace_na(list(total_rebate=0)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_total_rebate=mean(total_rebate), 
            sd_total_rebate = sd(total_rebate),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_total_rebate = sd_total_rebate / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_total_rebate,
           ymin = mean_total_rebate - 1.96*se_total_rebate,
           ymax = mean_total_rebate + 1.96*se_total_rebate)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average rebate claimed", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/rebate_vs_assessvalue_all.png", width=6, height=6)

# Program participation vs. assessed value
hd <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            participant = mean(treated)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_participate=mean(participant), 
            sd_participate = sd(participant),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_participate = sd_participate / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_participate,
           ymin = mean_participate - 1.96*se_participate,
           ymax = mean_participate + 1.96*se_participate)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Participation rate", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/participate_vs_assessvalue_all.png", width=6, height=6)

# Rebate vs. assessed value conditional on participation
hd <- rd %>% 
  filter(treated == TRUE) %>%
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            total_rebate = mean(total_paid_inc)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_total_rebate=mean(total_rebate, na.rm=T), 
            sd_total_rebate = sd(total_rebate, na.rm=T),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_total_rebate = sd_total_rebate / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_total_rebate,
           ymin = mean_total_rebate - 1.96*se_total_rebate,
           ymax = mean_total_rebate + 1.96*se_total_rebate)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average rebate claimed", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/rebate_vs_assessvalue_participantsonly.png", width=6, height=6)

# Histogram of assessed value
hd <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(avg_value = mean(TotalAssesmentValue),
            Count = n()) 

ggplot(hd,
       aes(x=avg_value,
           y=Count)) +
  geom_col() +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  theme_bw()
ggsave("../output_figures_tables/count_vs_assessvalue_all.png", width=6, height=6)

# TWFE estimates for full sample by assessed value
rd_stag_cuts <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue)) %>%
  inner_join(rd_stag) %>%
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val))

res_twfe_full_sample_annual_gas = feols(log(gas) ~ i(treated_post,assess_bin, ref=FALSE) | id + consyear^assess_bin, rd_stag_cuts  %>% filter(years_to_treatment != -2000), cluster=~id + consyear, combine.quick = FALSE)

# Predict energy consumption with and without retrofits
pred_gas_save <- rd_stag_cuts %>%
  mutate(consumption_with_retrofits = predict(res_twfe_full_sample_annual_gas, .)) %>%
  mutate(treated_post = FALSE) %>%
  mutate(consumption_without_retrofits = predict(res_twfe_full_sample_annual_gas, .)) %>%
  mutate(diff = consumption_with_retrofits - consumption_without_retrofits) %>%
  mutate(gas_savings_gj = gas * (- exp(diff) + 1)) %>%
# Assume all savings gas at $5.98/GJ (average pre-retrofit price); multiply by 12 for annual
  mutate(dollar_savings_gas = gas_savings_gj * 5.98 * 12)

hd <- pred_gas_save %>% 
  group_by(id) %>% 
  # Only keep observations for 2018
  filter(consyear == 2018) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_gas_savings=mean(dollar_savings_gas, na.rm=T), 
            sd_gas_savings = sd(dollar_savings_gas, na.rm=T),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_gas_savings = sd_gas_savings / sqrt(count))

ggplot(hd, 
       aes(x=avg_value,
           y=mean_gas_savings,
           ymin = mean_gas_savings - 1.96*se_gas_savings,
           ymax = mean_gas_savings + 1.96*se_gas_savings)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average annual gas savings", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/gas_save_vs_assessvalue_all.png", width=6, height=6)

hd <- pred_gas_save %>% 
  group_by(id) %>% 
  # Only keep observations for 2018
  filter(treated == TRUE) %>%
  filter(consyear == 2018) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_gas_savings=mean(dollar_savings_gas, na.rm=T), 
            sd_gas_savings = sd(dollar_savings_gas, na.rm=T),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_gas_savings = sd_gas_savings / sqrt(count))

ggplot(hd, 
       aes(x=avg_value,
           y=mean_gas_savings,
           ymin = mean_gas_savings - 1.96*se_gas_savings,
           ymax = mean_gas_savings + 1.96*se_gas_savings)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average annual gas savings", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/gas_save_vs_assessvalue_participantsonly.png", width=6, height=6)

# Savings for program participants
hd <- rd_stag_cuts %>%
  group_by(id, assess_bin) %>%
  summarise(TotalAssesmentValue = mean(TotalAssesmentValue)) %>%
  group_by(assess_bin) %>%
  summarise(TotalAssesmentValue = mean(TotalAssesmentValue)) %>%
  inner_join(res_twfe_full_sample_annual_gas %>%
              tidy() %>%
              rename(assess_bin = term) %>%
              mutate(assess_bin = gsub("treated_post::TRUE:assess_bin::","", assess_bin))
  )

ggplot(hd, aes(x=TotalAssesmentValue, y=-estimate, ymin=-estimate - 1.96*std.error, ymax=-estimate + 1.96*std.error)) +
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue")  +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average annual gas savings", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/gas_save_vs_assessvalue_percent_participantsonly.png", width=6, height=6)

# Percent savings for all
pred_gas_save <- rd_stag_cuts %>%
  mutate(consumption_with_retrofits = predict(res_twfe_full_sample_annual_gas, .)) %>%
  mutate(treated_post = FALSE) %>%
  mutate(consumption_without_retrofits = predict(res_twfe_full_sample_annual_gas, .)) %>%
  mutate(diff = consumption_with_retrofits - consumption_without_retrofits) %>%
  mutate(gas_savings_percent =  (- exp(diff) + 1))

hd <- pred_gas_save %>% 
  group_by(id) %>% 
  # Only keep observations for 2018
  filter(consyear == 2018) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_gas_savings=mean(gas_savings_percent, na.rm=T), 
            sd_gas_savings = sd(gas_savings_percent, na.rm=T),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_gas_savings = sd_gas_savings / sqrt(count))

ggplot(hd, 
       aes(x=avg_value,
           y=mean_gas_savings,
           ymin = mean_gas_savings - 1.96*se_gas_savings,
           ymax = mean_gas_savings + 1.96*se_gas_savings)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average annual gas savings", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)
ggsave("../output_figures_tables/gas_save_percent_vs_assessvalue_all.png", width=6, height=6)
