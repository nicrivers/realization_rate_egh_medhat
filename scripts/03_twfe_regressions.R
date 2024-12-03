# pre-treatment data for matching
# first pre-retrofit entrydate is  
# > min(dat$preretrofit_entrydate, na.rm=T)
# [1] "2008-01-21"
# conduct pre-treatment matching on pre-2008 data
pre_treat <- rd %>%
  filter(year < 2008) %>%
  dplyr::select(id, treated, gas, elec, consmonth) %>%
  mutate(season = 
           case_when(consmonth %in% c(12,1,2,3) ~ "winter",
                     consmonth %in% c(9,10,11,4,5) ~ "shoulder",
                     consmonth %in% c(6,7,8) ~ "summer")) %>%
  group_by(id, treated, season) %>%
  summarise(gas = mean(gas, na.rm=T),
            elec = mean(elec, na.rm=T)) %>%
  pivot_wider(names_from="season", values_from=c("gas","elec")) %>%
  drop_na()

# Merge data
pre_treat <- inner_join(pre_treat, taxdat)

# 1:1 propensity score matching without replacement on pre-treatment consumption
m.pre_treat <- matchit(treated ~ gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                  data=pre_treat, method="nearest", distance="glm")

# Matching on building characteristics only
m.build <- matchit(treated ~ Neighbourhood + log(BuildingSize) + log(TotalAssesmentValue) + EffectiveYearBuild + Building_Condition + Building_Type + umLocClass, 
                  data=pre_treat %>% filter(TotalAssesmentValue > 0, BuildingSize > 0), method="nearest", distance="glm")

# Matching on building characteristics and energy consumption
m.pt_b <- matchit(treated ~ Neighbourhood + log(BuildingSize) + log(TotalAssesmentValue) + EffectiveYearBuild + Building_Condition + Building_Type + umLocClass + gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                  data=pre_treat %>% filter(TotalAssesmentValue > 0, BuildingSize > 0), method="nearest", distance="glm")

# Samples for analysis
## 1. full sample
full_sample <- rd
## 2. participants + matched controls based on pre-treatment consumption
p_mc_cons <- match.data(m.pre_treat) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)
## 3. participants + matched controls based on building characteristics
p_mc_build <- match.data(m.build) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)
## 4. participants + matched controls based on building characteristics
p_mc_pt_b <- match.data(m.pt_b) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)
## 5. Participants only
partic_only <- rd %>%
  filter(treated == TRUE,
         # Keep only years with "good" comparisons
         year < 2012)


###########
# TWFE regression on total energy consumption with different samples
# Monthly data
###########

# Energy
m_fs_twfe_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=rd, cluster = ~id+cons_date)
m_p_mc_cons_twfe_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=p_mc_cons, cluster = ~id+cons_date, weights=~weights)
m_p_mc_build_twfe_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=p_mc_build, cluster = ~id+cons_date, weights=~weights)
m_p_mc_pt_b_twfe_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=p_mc_pt_b, cluster = ~id+cons_date, weight=~weights)
m_partic_twfe_energy <- feols(log(energy) ~ treated_post | id + cons_date , data=partic_only, cluster = ~id+cons_date)

etable(m_fs_twfe_energy,
       m_p_mc_cons_twfe_energy,
       m_p_mc_build_twfe_energy,
       m_p_mc_pt_b_twfe_energy,
       m_partic_twfe_energy, 
       tex=T, replace=T, file="../output_figures_tables/twfe_monthly_energy.tex")

# Gas
m_fs_twfe_gas <- feols(log(gas) ~ treated_post | id + cons_date , data=rd, cluster = ~id+cons_date)
m_p_mc_cons_twfe_gas <- feols(log(gas) ~ treated_post | id + cons_date , data=p_mc_cons, cluster = ~id+cons_date, weights=~weights)
m_p_mc_build_twfe_gas <- feols(log(gas) ~ treated_post | id + cons_date , data=p_mc_build, cluster = ~id+cons_date, weights=~weights)
m_p_mc_pt_b_twfe_gas <- feols(log(gas) ~ treated_post | id + cons_date , data=p_mc_pt_b, cluster = ~id+cons_date, weight=~weights)
m_partic_twfe_gas <- feols(log(gas) ~ treated_post | id + cons_date , data=partic_only, cluster = ~id+cons_date)

etable(m_fs_twfe_gas,
       m_p_mc_cons_twfe_gas,
       m_p_mc_build_twfe_gas,
       m_p_mc_pt_b_twfe_gas,
       m_partic_twfe_gas, 
       tex=T, replace=T, file="../output_figures_tables/twfe_monthly_gas.tex")

# Electricity
m_fs_twfe_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=rd, cluster = ~id+cons_date)
m_p_mc_cons_twfe_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=p_mc_cons, cluster = ~id+cons_date, weights=~weights)
m_p_mc_build_twfe_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=p_mc_build, cluster = ~id+cons_date, weights=~weights)
m_p_mc_pt_b_twfe_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=p_mc_pt_b, cluster = ~id+cons_date, weight=~weights)
m_partic_twfe_elec <- feols(log(elec) ~ treated_post | id + cons_date , data=partic_only, cluster = ~id+cons_date)

etable(m_fs_twfe_elec,
       m_p_mc_cons_twfe_elec,
       m_p_mc_build_twfe_elec,
       m_p_mc_pt_b_twfe_elec,
       m_partic_twfe_elec, 
       tex=T, replace=T, file="../output_figures_tables/twfe_monthly_elec.tex")


##########
# Sun and Abraham correction
##########
# Sun and Abraham is estimated by interacting a "cohort" dummy with a "time to treatment" dummy
# With monthly data, we have 204 unique "time to treatment" and 49 unique "cohort".  This implies
# 49*204 = 9996 unique dummies to estimate.  This is too large a matrix to invert.
# Instead, we use annual data.
month_since_2000 <- tibble(expand_grid(year = seq(2000,2020), 
                                       month = seq(1,12))) %>%
  mutate(month_index = row_number())

dd <- partic_only %>%
  inner_join(month_since_2000 %>%
               rename(cons_index = month_index), by=c("consyear" = "year",
                                    "consmonth" = "month")) %>%
  mutate(post_retrofit_year = year(postretrofit_entrydate),
         post_retrofit_month = month(postretrofit_entrydate)) %>%
  inner_join(month_since_2000 %>%
               rename(postretrofit_index = month_index), by=c("post_retrofit_year" = "year",
                                                              "post_retrofit_month" = "month")) %>%
  mutate(pre_retrofit_year = year(preretrofit_entrydate),
         pre_retrofit_month = month(preretrofit_entrydate)) %>%
  inner_join(month_since_2000 %>%
               rename(preretrofit_index = month_index), by=c("pre_retrofit_year" = "year",
                                                              "pre_retrofit_month" = "month")) %>%
  mutate(months_to_treatment = case_when(
           cons_index < preretrofit_index ~ cons_index - preretrofit_index,
           cons_index > postretrofit_index ~ cons_index - postretrofit_index,
           #is.na(retrofit_end_year) ~ -1000,
           cons_index >= preretrofit_index & cons_index <= postretrofit_index ~ -2000
         ))
length(unique(dd$months_to_treatment))
length(unique(dd$postretrofit_index))

######################################################
# Set up annual data

# Samples for analysis
## 1. full sample
full_sample_annual <- rd_stag
## 2. participants + matched controls based on pre-treatment consumption
p_mc_cons_annual <- match.data(m.pre_treat) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd_stag)
## 3. participants + matched controls based on building characteristics
p_mc_build_annual <- match.data(m.build) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd_stag)
## 4. participants + matched controls based on building characteristics
p_mc_pt_b_annual <- match.data(m.pt_b) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd_stag)
## 5. Participants only
partic_only_annual <- rd_stag %>%
  filter(treated == TRUE,
         # Keep only observations with "good" comparisons
         consyear < 2012)

# All energy
res_sunab_full_sample = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, full_sample_annual %>% 
                    filter(years_to_treatment != -2000) %>% 
                    mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)), cluster=~id + consyear)

res_sunab_m_pt = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_cons_annual %>% 
                                filter(years_to_treatment != -2000) %>% 
                                mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                       weights = ~weights, cluster=~id + consyear)

res_sunab_m_build = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_build_annual %>% 
                         filter(years_to_treatment != -2000) %>% 
                         mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                       weights = ~weights, cluster=~id + consyear)

res_sunab_m_pt_b = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_pt_b_annual %>% 
                         filter(years_to_treatment != -2000) %>% 
                         mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                       weights = ~weights, cluster=~id + consyear)

res_sunab_partic = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, partic_only_annual %>% 
                         filter(years_to_treatment != -2000), cluster=~id + consyear)

# Keeping only years where not everyone is treated
res_sunab_partic_notall = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, partic_only_annual %>% 
                           filter(years_to_treatment != -2000, consyear < 2012), cluster=~id + consyear)

etable(res_sunab_full_sample, res_sunab_m_pt, res_sunab_m_build, res_sunab_m_pt_b, res_sunab_partic, agg = "att", 
       tex=T, replace=T, file="../output_figures_tables/sunab_annual_energy.tex")

# Same annual analysis with TWFE
res_twfe_full_sample_annual = feols(log(energy) ~ treated_post | id + consyear, full_sample_annual %>% filter(years_to_treatment != -2000), cluster=~id + consyear)
                             
res_twfe_m_pt_annual = feols(log(energy) ~ treated_post | id + consyear, p_mc_cons_annual %>% filter(years_to_treatment != -2000), weights = ~weights, cluster=~id + consyear)

res_twfe_m_build_annual = feols(log(energy) ~ treated_post | id + consyear, p_mc_build_annual %>% filter(years_to_treatment != -2000), weights = ~weights, cluster=~id + consyear)

res_twfe_m_pt_b_annual = feols(log(energy) ~ treated_post | id + consyear, p_mc_pt_b_annual %>% filter(years_to_treatment != -2000), weights = ~weights, cluster=~id + consyear)

res_twfe_partic_annual = feols(log(energy) ~ treated_post | id + consyear, partic_only_annual %>% filter(years_to_treatment != -2000), cluster=~id + consyear)

etable(res_twfe_full_sample_annual, res_twfe_m_pt_annual, res_twfe_m_build_annual, res_twfe_m_pt_b_annual, res_twfe_partic_annual, 
       tex=T, replace=T, file="../output_figures_tables/twfe_annual_energy.tex")

# Gas
res_sunab_full_sample_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, full_sample_annual %>% 
                                filter(years_to_treatment != -2000) %>% 
                                mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)), cluster=~id + consyear)

res_sunab_m_pt_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_cons_annual %>% 
                         filter(years_to_treatment != -2000) %>% 
                         mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                       weights = ~weights, cluster=~id + consyear)

res_sunab_m_build_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_build_annual %>% 
                            filter(years_to_treatment != -2000) %>% 
                            mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                          weights = ~weights, cluster=~id + consyear)

res_sunab_m_pt_b_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_pt_b_annual %>% 
                           filter(years_to_treatment != -2000) %>% 
                           mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                         weights = ~weights, cluster=~id + consyear)

res_sunab_partic_gas = feols(log(gas) ~ sunab(retrofit_end_year, consyear) | id + consyear, partic_only_annual %>% 
                           filter(years_to_treatment != -2000), cluster=~id + consyear)

etable(res_sunab_full_sample_gas, res_sunab_m_pt_gas, res_sunab_m_build_gas, res_sunab_m_pt_b_gas, res_sunab_partic_gas, agg = "att", 
       tex=T, replace=T, file="../output_figures_tables/sunab_annual_gas.tex")

# Electricity
res_sunab_full_sample_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, full_sample_annual %>% 
                                    filter(years_to_treatment != -2000) %>% 
                                    mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)), cluster=~id + consyear)

res_sunab_m_pt_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_cons_annual %>% 
                             filter(years_to_treatment != -2000) %>% 
                             mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                           weights = ~weights, cluster=~id + consyear)

res_sunab_m_build_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_build_annual %>% 
                                filter(years_to_treatment != -2000) %>% 
                                mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                              weights = ~weights, cluster=~id + consyear)

res_sunab_m_pt_b_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, p_mc_pt_b_annual %>% 
                               filter(years_to_treatment != -2000) %>% 
                               mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)),
                             weights = ~weights, cluster=~id + consyear)

# Remove the period when all units are treated
res_sunab_partic_elec = feols(log(elec) ~ sunab(retrofit_end_year, consyear) | id + consyear, partic_only_annual %>% 
                               filter(years_to_treatment != -2000, consyear < 2012), cluster=~id + consyear)

etable(res_sunab_full_sample_elec, res_sunab_m_pt_elec, res_sunab_m_build_elec, res_sunab_m_pt_b_elec, res_sunab_partic_elec, agg = "att", 
       tex=T, replace=T, file="../output_figures_tables/sunab_annual_elec.tex")


# Robustness checks to drop zero and near-zero consumption
res_twfe_full_sample_annual = feols(log(energy) ~ treated_post | id + consyear, full_sample_annual %>% filter(years_to_treatment != -2000), cluster=~id + consyear)

# Drop periods where energy consumption is below 1st percentile
p1_sample <- full_sample_annual %>% ungroup() %>% filter(years_to_treatment != -2000, !is.na(energy)) %>% mutate(limit = quantile(energy,probs = 0.01)) %>% filter(energy >= limit)
res_twfe_full_sample_annual_p1 = feols(log(energy) ~ treated_post | id + consyear, p1_sample, cluster=~id + consyear)

# Drop periods where energy consumption is below 5th percentile
p5_sample <- full_sample_annual %>% ungroup() %>% filter(years_to_treatment != -2000, !is.na(energy)) %>% mutate(limit = quantile(energy,probs = 0.05)) %>% filter(energy >= limit)
res_twfe_full_sample_annual_p5 = feols(log(energy) ~ treated_post | id + consyear, p5_sample, cluster=~id + consyear)



etable(res_twfe_full_sample_annual, res_twfe_full_sample_annual_p1, res_twfe_full_sample_annual_p5, tex=T,
       replace=T, file="../output_figures_tables/twfe_drop_low.tex")


