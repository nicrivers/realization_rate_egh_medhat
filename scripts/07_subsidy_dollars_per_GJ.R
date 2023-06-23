# Use annual data
rd_year <- rd %>%
  group_by(id,year) %>%
  summarise(energy=sum(energy, na.rm=T),
            treated_post = mean(treated_post)) %>%
  # Only keep when treatment status is clear
  filter(treated_post == 0 | treated_post == 1)

# Regression with energy in levels (GJ)
m1_all_energy <- feols(energy ~ treated_post | id + year , data=rd_year, cluster = ~id+year)

# Average rebate received
reb <- rd %>%
  group_by(id) %>%
  summarise(rebate = mean(total_paid_inc, na.rm=T),
            predicted_savings = mean(predicted_preretrofit_energy_gj_per_yr - predicted_postretrofit_energy_gj_per_yr)) %>%
  filter(!is.nan(rebate)) %>%
  ungroup() %>%
  summarise(total_rebates = sum(rebate),
            num_houses = n(),
            predicted_savings = mean(predicted_savings)) %>%
  mutate(average_rebate = total_rebates / num_houses)

# Dollars per GJ
reb <- reb %>%
  mutate(savings = - coef(m1_all_energy),
         dollar_per_annual_gj_realized = average_rebate / savings,
         dollar_per_annual_gj_projected = average_rebate / predicted_savings) %>%
  dplyr::select(average_rebate, contains("dollar"))

print(xtable::xtable(reb), "../output_figures_tables/dollar_savings.tex", type="latex")
