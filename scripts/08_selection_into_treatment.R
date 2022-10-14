# This analysis evaluates selection into treatment by using the ever-treated sample
# In addition, we conduct matching based on furnace type (a proxy for age)


# First, run analysis using the ever-treated sample. Stop at 2011. Households that eventually get treated but
# aren't treated until 2011 represent control group
rd_ever_treated <- rd %>%
  filter(treated == TRUE) %>%
  filter(year < 2012)

m1_ever_treated_gas <- feols(log(gas) ~ treated_post | id + cons_date, data=rd_ever_treated, cluster = ~id+cons_date)
m1_ever_treated_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=rd_ever_treated, cluster = ~id+cons_date)
m1_ever_treated_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd_ever_treated, cluster = ~id+cons_date)

etable(m1_ever_treated_gas, m1_ever_treated_elec, m1_ever_treated_energy)
etable(list(m1_ever_treated_gas, m1_ever_treated_elec, m1_ever_treated_energy), tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated.tex", replace = TRUE)

m1_ever_treated_2012_energy <- feols(log(energy) ~ treated_post | id + cons_date, data=rd_ever_treated, cluster = ~id+cons_date)
m1_ever_treated_2011_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd_ever_treated %>% filter(year < 2011), cluster = ~id+cons_date)
m1_ever_treated_2010_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd_ever_treated %>% filter(year < 2010), cluster = ~id+cons_date)

etable(m1_ever_treated_2012_energy, m1_ever_treated_2011_energy, m1_ever_treated_2010_energy)
etable(list(m1_ever_treated_2012_energy, m1_ever_treated_2011_energy, m1_ever_treated_2010_energy), tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated_diff_years.tex", replace = TRUE)

# Run the event study estimator with this sample
rd_stag <- rd_ever_treated %>%
  mutate(retrofit_start_year = year(preretrofit_entrydate),
         retrofit_end_year = year(postretrofit_entrydate),
         years_to_treatment = case_when(
           consyear < retrofit_start_year ~ consyear - retrofit_start_year,
           consyear > retrofit_end_year ~ consyear - retrofit_end_year,
           is.na(retrofit_end_year) ~ -1000,
           consyear >= retrofit_start_year & consyear <= retrofit_end_year ~ -2000
         )) %>%
  group_by(id,consyear,treated,post,treated_post,years_to_treatment,retrofit_start_year, retrofit_end_year) %>%
  summarise(elec=mean(elec, na.rm=T),
            gas=mean(gas, na.rm=T),
            energy=mean(energy, na.rm=T))

res_sunab_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag %>% 
                    filter(years_to_treatment != -2000) %>% 
                    mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)))
res_sunab_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag %>% 
                        filter(years_to_treatment != -2000) %>% 
                        mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)))
res_sunab_energy = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag %>% 
                        filter(years_to_treatment != -2000) %>% 
                        mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)))


etable(list(res_sunab_gas, res_sunab_elec, res_sunab_energy), agg = "att")
etable(list(res_sunab_gas, res_sunab_elec, res_sunab_energy), agg = "att", tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated_sunab.tex", replace = TRUE)

# Match on pre-retrofit furnace type
# Use the ever-treated group
# Drop all years after 2011
# People who retrofitted in 2012 or after are the control group
rd_ever_treated_late_control <- rd_ever_treated %>%
  mutate(treated = year(postretrofit_entrydate) < 2012) %>%
  group_by(id,pre_retrofit_furnacetype) %>%
  summarise(treated=mean(treated))

ever_treated_match <- matchit(treated ~ pre_retrofit_furnacetype,
                              data=rd_ever_treated_late_control,
                              method="exact")

match_data_ever_treated <- match.data(ever_treated_match) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd_ever_treated)

m1_ever_treated_match_gas <- feols(log(gas) ~ treated_post | id + cons_date, data=match_data_ever_treated, cluster = ~id+cons_date, weights = match_data_ever_treated$weights)
m1_ever_treated_match_elec <- feols(log(elec) ~ treated_post | id + cons_date, data=match_data_ever_treated, cluster = ~id+cons_date, weights = match_data_ever_treated$weights)
m1_ever_treated_match_energy <- feols(log(energy) ~ treated_post | id + cons_date, data=match_data_ever_treated, cluster = ~id+cons_date, weights = match_data_ever_treated$weights)

etable(list(m1_ever_treated_match_gas, m1_ever_treated_match_elec, m1_ever_treated_match_energy), tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated_sunab.tex", replace = TRUE)


# In event study form, with Sun and Abraham estimation

rd_stag_ever_treated <- match.data(ever_treated_match) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd_stag) %>%
  filter(years_to_treatment != -2000)

res_sunab_match_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag_ever_treated %>% 
                    filter(years_to_treatment != -2000) %>% 
                    mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                    weights = rd_stag_ever_treated$weights)
res_sunab_match_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag_ever_treated %>% 
                              filter(years_to_treatment != -2000) %>% 
                              mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                            weights = rd_stag_ever_treated$weights)
res_sunab_match_energy = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag_ever_treated %>% 
                              filter(years_to_treatment != -2000) %>% 
                              mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                            weights = rd_stag_ever_treated$weights)

etable(list(res_sunab_match_gas, res_sunab_match_elec, res_sunab_match_energy), agg = "att")
etable(list(res_sunab_match_gas, res_sunab_match_elec, res_sunab_match_energy), agg = "att", tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated_match_sunab.tex", replace = TRUE)
