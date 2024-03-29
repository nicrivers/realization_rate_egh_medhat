# Regression analysis for overall energy savings from EGH retrofit


#### 
# Regression analysis with ever-treated households only
# Control group will be households that didn't get treated until 2012
# This means that I drop years after and including 2012

rd_ever_treated <- rd %>%
  filter(treated == TRUE) %>%
  filter(year < 2012)

m1_ever_treated_gas <- feols(log(gas) ~ treated_post | id + cons_date, data=rd_ever_treated, cluster = ~id+cons_date)
m1_ever_treated_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=rd_ever_treated, cluster = ~id+cons_date)
m1_ever_treated_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd_ever_treated, cluster = ~id+cons_date)

etable(m1_ever_treated_gas, m1_ever_treated_elec, m1_ever_treated_energy)

####
# Regression analysis with non-treated households
# Control group will be not-yet treated households an never-treated households
# To compare with prior results I drop years after and including 2012

rd_w_never_treated <- rd %>%
  filter(year < 2012)

m1_w_never_treated_gas <- feols(log(gas) ~ treated_post | id + cons_date, data=rd_w_never_treated, cluster = ~id+cons_date)
m1_w_never_treated_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=rd_w_never_treated, cluster = ~id+cons_date)
m1_w_never_treated_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd_w_never_treated, cluster = ~id+cons_date)

etable(m1_w_never_treated_gas, m1_w_never_treated_elec, m1_w_never_treated_energy)

####
# Regression analysis with non-treated households
# Control group will be not-yet treated households an never-treated households
# Now I include years after 2012 for a long-run analysis

m1_all_gas <- feols(log(gas) ~ treated_post | id + cons_date, data=rd, cluster = ~id+cons_date)
m1_all_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=rd, cluster = ~id+cons_date)
m1_all_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd, cluster = ~id+cons_date)

etable(m1_all_gas, m1_all_elec, m1_all_energy)
etable(list(m1_ever_treated_energy, m1_w_never_treated_energy, m1_all_energy), tex=TRUE, file="../output_figures_tables/fuels_regression_never_treated.tex", replace = TRUE)

# Compare the estimates for total energy
etable(m1_ever_treated_energy, m1_w_never_treated_energy, m1_all_energy)
etable(list(m1_ever_treated_energy, m1_w_never_treated_energy, m1_all_energy), tex=TRUE, file="../output_figures_tables/main_energy_regression_diff_samples.tex", replace = TRUE)
# It looks like there is a difference between the results with never-treated households
# and the results that only include the ever-treated households
# Look at the event study results to understand why. The only-treated event study
# shows that these estimates are likely biased. This is bc we are comparing
# newly treated households to previously treated households with the ever-treated sample.

## Compare with house-month fixed effects
m1_all_energy_hm <- feols(log(energy) ~ treated_post | id^consmonth + cons_date , data=rd, cluster = ~id+cons_date)
etable(m1_all_energy,m1_all_energy_hm, tex=TRUE, file="../output_figures_tables/house_month_fe.tex", title = "Regression with house-month fixed effects\\label{tab:hm}", replace = TRUE)

## Output for paper
etable(list(m1_all_gas, m1_all_elec, m1_all_energy), tex=TRUE, file="../output_figures_tables/main_twfe_regression.tex", title = "Main panel regression\\label{tab:maintwfe}", replace = TRUE)


## Summary statistics
# house characteristics
st(data = rd %>%
     group_by(id) %>%
     # For non-participants, replace NA with 0
     mutate(across(contains("done"), ~replace_na(.,0))) %>%
     summarise(across(c(contains("done"),contains("preretrofit"),contains("postretrofit")), mean)) %>%
     dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
     rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)), 
   file = "../output_figures_tables/house_sumtable.tex", out="latex")

# energy consumption characteristics
st(data = rd %>% mutate(participant = !is.na(postretrofit_naturalgasconsum)) %>%
     dplyr::select(participant, elec, gas, energy), group="participant",
   file = "../output_figures_tables/month_sumtable.tex", out="latex")

# Formatted summary table
st(data = rd %>%
     group_by(id) %>%
     # For non-participants, replace NA with 0
     mutate(across(contains("done"), ~replace_na(.,0))) %>%
     summarise(across(c(contains("done"),contains("gj_per_yr"), contains("value"), contains("size"), contains("yearbuild")), mean, na.rm=T)) %>%
     dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
     rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
     mutate(participant = !is.na(predicted_postretrofit_gas_gj_per_yr)),
   group = "participant",
   file = "../output_figures_tables/both_sumtable.tex",out="latex",
   title = "Summary statistics\\label{tab:sumstat}")

# Try a better-formatted table using gtsummary
### by defaul, tbl_summary produces median and IQR
tbl_summary(rd %>%
     group_by(id) %>%
     # For non-participants, replace NA with 0
     mutate(across(contains("done"), ~replace_na(.,0))) %>%
     summarise(across(c(contains("done"),contains("gj_per_yr"), contains("value"), contains("size"), contains("yearbuild")), mean, na.rm=T)) %>%
     dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
     rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
     mutate(participant = !is.na(predicted_postretrofit_gas_gj_per_yr)),
   by = "participant")

# This is nice, but full support for LaTeX is still not available for gt objects

sum_dat <- rd %>%
  group_by(id) %>%
  # For non-participants, replace NA with 0
  mutate(across(contains("done"), ~replace_na(.,0))) %>%
  summarise(across(c(contains("done"),contains("gj_per_yr"), contains("value"), contains("size"), contains("yearbuild")), mean, na.rm=T)) %>%
  dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
  rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
  mutate(participant = !is.na(predicted_postretrofit_gas_gj_per_yr))

t1 <- tbl_summary(sum_dat %>% 
                    dplyr::select(contains("actual"), participant),
                  by="participant") %>%
  add_p()

t2 <- tbl_summary(sum_dat %>% 
                    dplyr::select(air_sealing, ceiling_insulation, walls_insulation, bsmt_insulation, fnd_header, windowsand_doors, central_ac, natural_gas_furnace, participant),
                  by="participant")

t3 <- tbl_summary(sum_dat %>% 
                    dplyr::select(TotalAssesmentValue, LotSize, BuildingSize, EffectiveYearBuild, participant),
                  by="participant") %>%
  add_p()

t4 <- tbl_summary(sum_dat %>% 
                    dplyr::select(contains("predicted"), participant),
                  by="participant") 


#tbl_stack(list(t1,t4,t2,t3),
#                         "Predicted energy consumption",
#                           "Retrofits undertaken",
#                           "Tax assessment data")) %>%
#  modify_footnote(all_stat_cols() ~ "median (IQR) for actual and predicted energy and tax data; n (%) for retrofit measures") %>%
#  as_gt() %>% gt::gtsave("../output_figures_tables/both_sumtable.tex")
