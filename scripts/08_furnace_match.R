# Matching on furnace type

# First, run analysis using the ever-treated sample. Stop at 2011. Households that eventually get treated but
# aren't treated until 2011 represent control group
rd_ever_treated <- rd %>%
  filter(treated == TRUE) %>%
  filter(year < 2012)

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

etable(list(m1_ever_treated_match_gas, m1_ever_treated_match_elec, m1_ever_treated_match_energy), tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated_furnace_match.tex", replace = TRUE)

# In event study form, with Sun and Abraham estimation

rd_stag_ever_treated <- match.data(ever_treated_match) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd_stag) %>%
  filter(years_to_treatment != -2000)

res_sunab_match_gas = feols(log(gas) ~ sunab(retrofit_start_year, consyear) | id + consyear, rd_stag_ever_treated %>% 
                              filter(years_to_treatment != -2000, consyear < 2012) %>% 
                              mutate(retrofit_start_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                            weights = ~weights)
res_sunab_match_elec = feols(log(elec) ~ sunab(retrofit_start_year, consyear) | id + consyear, rd_stag_ever_treated %>% 
                               filter(years_to_treatment != -2000, consyear < 2012) %>% 
                               mutate(retrofit_start_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                             weights = ~weights)
res_sunab_match_energy = feols(log(energy) ~ sunab(retrofit_start_year, consyear) | id + consyear, rd_stag_ever_treated %>% 
                                 filter(years_to_treatment != -2000, consyear < 2012) %>% 
                                 mutate(retrofit_start_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                               weights = ~weights)

etable(list(res_sunab_match_gas, res_sunab_match_elec, res_sunab_match_energy), agg = "att")
etable(list(res_sunab_match_gas, res_sunab_match_elec, res_sunab_match_energy), agg = "att", tex=TRUE, file="../output_figures_tables/fuels_regression_ever_treated_furnace_match_sunab.tex", replace = TRUE)
