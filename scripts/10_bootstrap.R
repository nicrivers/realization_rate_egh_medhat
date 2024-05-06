# Must run 09_distributional_graphs.R first
# This uses variables described in that file.

# Do the bootstrap
all_ids <- unique(rd$id)

all_results <- tibble()
n_replications = 100

# average values
hv <- rd %>% 
  group_by(id) %>% 
  summarise(TotalAssesmentValue=mean(TotalAssesmentValue)) %>%
  inner_join(rd_stag) %>%
  mutate(assess_bin = cut(TotalAssesmentValue, breaks_val)) |>
  group_by(assess_bin) |>
  summarise(avg_value = mean(TotalAssesmentValue))

for (i in 1:n_replications) {

sample_ids <- sample(all_ids, size=length(all_ids), replace=TRUE)

rd_samp <- rd %>%
  filter(id %in% sample_ids)

# TWFE estimates for full sample by assessed value
rd_stag_cuts <- rd_samp %>% 
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
  # Only keep observations for 2018
  filter(consyear == 2015) %>% 
  group_by(assess_bin, treated) %>% 
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
  summarise(mean_gas_savings=mean(dollar_savings_gas, na.rm=T),
            mean_percent_savings=mean(diff, na.rm=T),
            mean_gas_with_retrofit=mean(consumption_with_retrofits),
            mean_gas_without_retrofit=mean(consumption_without_retrofits),
            mean_bill_percent_savings=mean(percent_bill_save),
            n=n()) %>%
  mutate(replication_number = i)

all_results <- bind_rows(all_results, hd)
}

# Percent gas bill savings for participants only
all_results |>
  inner_join(hv) |>
  filter(treated == TRUE) |>
  group_by(assess_bin,avg_value) |>
  summarise(se = sd(mean_bill_percent_savings),
            mean_bill_save_percent = -mean(mean_bill_percent_savings)) |>
  ggplot(
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
ggsave("../output_figures_tables/bill_save_percent_vs_assessvalue_all_bootstrap.png", width=6, height=6)

# Dollar savings for participants only
all_results |>
  inner_join(hv) |>
  filter(treated == TRUE) |>
  group_by(assess_bin,avg_value) |>
  summarise(se = sd(mean_gas_savings),
            mean_savings = mean(mean_gas_savings)) |>
  ggplot(
    aes(x=avg_value,
        y=mean_savings,
        ymin = mean_savings - 1.96*se,
        ymax = mean_savings + 1.96*se)) + 
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.1, fill="blue") +
  scale_x_continuous(name="Average assessed value", labels=scales::dollar_format()) +
  scale_y_continuous(name="Average gas bill savings", labels=scales::dollar_format()) +
  theme_bw() +
  coord_cartesian(ylim=c(0,NA)) +
  geom_hline(yintercept = 0)  +
  geom_vline(xintercept = mean_house_val, colour="red", linetype="dashed")
ggsave("../output_figures_tables/gas_dollar_save_vs_assessvalue_participants_bootstrap.png", width=6, height=6)
