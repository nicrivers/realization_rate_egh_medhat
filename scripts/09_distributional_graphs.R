# Histogram of rebate received vs. home assessed value

# Where will the breaks be in assessed value to create bins
breaks_val <- c(-Inf, 5e4, 1e5, 1.25e5, 1.5e5, 1.75e5, 2e5, 2.25e5, 2.5e5, 2.75e5, 3e5, 3.5e5, 4e5, 5e5, Inf)

# Mean house value
mean_house_val <- rd %>% 
  group_by(id) %>% 
  summarise(avg_value=mean(TotalAssesmentValue)) %>%
  ungroup() %>%
  summarise(mean_val = mean(avg_value)) %>%
  pull()

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
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
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
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
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
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
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
  theme_bw()  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/count_vs_assessvalue_all.png", width=6, height=6)

# As density instead
hd <- rd %>% 
  group_by(id) %>% 
  summarise(avg_value=mean(TotalAssesmentValue))

ggplot(hd %>% filter(avg_value < 5e5),
       aes(x=avg_value)) +
  geom_density() +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  theme_bw()

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
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
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
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
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
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
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
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/gas_save_percent_vs_assessvalue_all.png", width=6, height=6)


# Estimated percent gas bill savings for program participants only
hd <- pred_gas_save %>%
  filter(treated == TRUE, 
         consyear == 2015) %>%
  mutate(monthly_fixed_charge = 18.50,
         threshold = 20.86,
         gas_without_retrofit = gas * (1 - diff),
         below_threshold_with_retrofit = case_when(gas < threshold ~ gas,
                                     gas >= threshold ~ threshold),
         above_threshold_with_retrofit = case_when(gas < threshold ~ 0,
                                     gas >= threshold ~ gas - threshold),
         variable_charge_with_retrofit = 6.22 * below_threshold_with_retrofit + (6.22+1.01) * above_threshold_with_retrofit,
         below_threshold_without_retrofit = case_when(gas_without_retrofit < threshold ~ gas_without_retrofit,
                                                      gas_without_retrofit >= threshold ~ threshold),
         above_threshold_without_retrofit = case_when(gas_without_retrofit < threshold ~ 0,
                                                   gas_without_retrofit >= threshold ~ gas_without_retrofit - threshold),
         variable_charge_without_retrofit = 6.22 * below_threshold_without_retrofit + (6.22+1.01) * above_threshold_without_retrofit,
         percent_bill_save = (monthly_fixed_charge + variable_charge_with_retrofit) / (monthly_fixed_charge + variable_charge_without_retrofit) - 1) %>%
          group_by(assess_bin) %>% 
          summarise(mean_bill_save_percent=-mean(percent_bill_save, na.rm=T), 
            sd_bill_save = sd(percent_bill_save, na.rm=T),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se = sd_bill_save / sqrt(count))

ggplot(hd, 
       aes(x=avg_value,
           y=mean_bill_save_percent,
           ymin = mean_bill_save_percent - 1.96*se,
           ymax = mean_bill_save_percent + 1.96*se)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average percent gas bill savings", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/bill_save_percent_vs_assessvalue_all.png", width=6, height=6)


#### MEASURE BY MEASURE ANALYSIS
hd <- rd %>% 
  group_by(id) %>%
  mutate(crap_furnace = as.numeric(pre_retrofit_furnacetype == "Furnace with continuous pilot")) %>%
  filter(treated == TRUE) %>%
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            crap_furnace = mean(crap_furnace, na.rm=T)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_crap_furnace=mean(crap_furnace), 
            sd_crap_furnace = sd(crap_furnace),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_crap_furnace = sd_crap_furnace / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_crap_furnace,
           ymin = mean_crap_furnace - 1.96*se_crap_furnace,
           ymax = mean_crap_furnace + 1.96*se_crap_furnace)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Continuous pilot furnace", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/crap_furnaces.png", width=6, height=6)

# good furnace
hd <- rd %>% 
  group_by(id) %>%
  mutate(good_furnace = as.numeric(pre_retrofit_furnacetype == "Condensing furnace")) %>%
  filter(treated == TRUE) %>%
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            good_furnace = mean(good_furnace, na.rm=T)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_good_furnace=mean(good_furnace), 
            sd_good_furnace = sd(good_furnace),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_good_furnace = sd_good_furnace / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_good_furnace,
           ymin = mean_good_furnace - 1.96*se_good_furnace,
           ymax = mean_good_furnace + 1.96*se_good_furnace)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Condensing furnace", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/good_furnaces.png", width=6, height=6)

# Air sealing
hd <- rd %>% 
  group_by(id) %>%
  filter(treated == TRUE) %>%
  replace_na(list(air_sealing_ugr_done=0)) %>%
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            air_seal = mean(air_sealing_ugr_done)) %>% 
  replace_na(list(total_rebate=0)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_air_seal=mean(air_seal), 
            sd_air_seal = sd(air_seal),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_air_seal = sd_air_seal / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_air_seal,
           ymin = mean_air_seal - 1.96*se_air_seal,
           ymax = mean_air_seal + 1.96*se_air_seal)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Air sealing completed", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/who_did_air_sealing.png", width=6, height=6)

# attic insulation
hd <- rd %>% 
  group_by(id) %>%
  filter(treated == TRUE) %>%
  replace_na(list(ceiling_insulation_ugr_done=0)) %>%
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue),
            ceiling = mean(ceiling_insulation_ugr_done)) %>% 
  replace_na(list(total_rebate=0)) %>% 
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) %>% 
  group_by(assess_bin) %>% 
  summarise(mean_ceiling=mean(ceiling), 
            sd_ceiling = sd(ceiling),
            count = n(),
            avg_value = mean(TotalAssesmentValue)) %>%
  mutate(se_ceiling = sd_ceiling / sqrt(count))


ggplot(hd, 
       aes(x=avg_value,
           y=mean_ceiling,
           ymin = mean_ceiling - 1.96*se_ceiling,
           ymax = mean_ceiling + 1.96*se_ceiling)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Attic upgrade completed", labels=scales::percent_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/who_did_attic_insulation.png", width=6, height=6)


