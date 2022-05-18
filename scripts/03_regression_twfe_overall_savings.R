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
etable(list(m1_ever_treated_energy, m1_w_never_treated_energy, m1_all_energy), tex=TRUE, file="../output_figures_tables/fuels_regression_never_treated.tex")

# Compare the estimates for total energy
etable(m1_ever_treated_energy, m1_w_never_treated_energy, m1_all_energy)
etable(list(m1_ever_treated_energy, m1_w_never_treated_energy, m1_all_energy), tex=TRUE, file="../output_figures_tables/main_energy_regression_diff_samples.tex")
# It looks like there is a difference between the results with never-treated households
# and the results that only include the ever-treated households
# Look at the event study results to understand why. The only-treated event study
# shows that these estimates are likely biased. This is bc we are comparing
# newly treated households to previously treated households with the ever-treated sample.
