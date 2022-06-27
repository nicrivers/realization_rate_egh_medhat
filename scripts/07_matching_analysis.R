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

# Match on pre-treatment energy consumption
balance_unmatched <- matchit(treated ~ gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                             data=pre_treat, method=NULL, distance="glm")

summary(balance_unmatched)
# Treated houses consume more gas

# 1:1 propensity score matching without replacement
m.out1 <- matchit(treated ~ gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                  data=pre_treat, method="nearest", distance="glm")

# Subclassification on a logistic PS with 10 subclasses after discarding controls outside common support of PS
m.out2 <- matchit(treated ~ gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter,
                  data=pre_treat, method="subclass", distance="glm",
                  discard = "control", subclass=10)

# Matching on building characteristics only
m.out3 <- matchit(treated ~ Neighbourhood + BuildingSize + log(TotalAssesmentValue) + EffectiveYearBuild + Building_Condition + Building_Type + umLocClass, 
                  data=pre_treat %>% filter(TotalAssesmentValue > 0), method="nearest", distance="glm")

# Matching on building characteristics and energy consumption
m.out4 <- matchit(treated ~ log(BuildingSize) + log(TotalAssesmentValue) + EffectiveYearBuild + gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                  exact = ~ Neighbourhood + Building_Type + umLocClass + Building_Condition,
                  data=pre_treat %>% filter(TotalAssesmentValue > 0, BuildingSize > 0), method="nearest", distance="glm")

# Same thing but with subclass analysis
m.out5 <- matchit(treated ~ Neighbourhood + Building_Type + umLocClass + Building_Condition + log(BuildingSize) + log(TotalAssesmentValue) + EffectiveYearBuild + gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                  data=pre_treat %>% filter(TotalAssesmentValue > 0, BuildingSize > 0), method="subclass", distance="glm", discard = "control", subclass = 10)

# much closer balance
summary(m.out1, un=FALSE)
summary(m.out2, un=FALSE)
summary(m.out3, un=FALSE)

# see matching
plot(m.out1, type = "jitter", interactive = FALSE)
plot(m.out1, type = "qq", interactive = FALSE, which.xs = c("gas_winter", "elec_summer"))

# Now we can estimate treatment effect on this matched sample
match_data1 <- match.data(m.out1) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)

match_data2 <- match.data(m.out2) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)

match_data3 <- match.data(m.out3) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)

match_data4 <- match.data(m.out4) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)

match_data5 <- match.data(m.out5) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)

m1_all_energy_nomatch <- feols(log(energy) ~ treated_post | id + cons_date, data=rd, cluster = ~id+cons_date)
m1_all_energy_match <- feols(log(energy) ~ treated_post | id + cons_date, data=match_data1, cluster = ~id+cons_date, weights = match_data1$weights)
m2_all_energy_match <- feols(log(energy) ~ treated_post | id + cons_date, data=match_data2, cluster = ~id+cons_date, weights = match_data2$weights)
m3_all_energy_match <- feols(log(energy) ~ treated_post | id + cons_date, data=match_data3, cluster = ~id+cons_date, weights = match_data3$weights)
m4_all_energy_match <- feols(log(energy) ~ treated_post | id + cons_date, data=match_data4, cluster = ~id+cons_date, weights = match_data4$weights)
m5_all_energy_match <- feols(log(energy) ~ treated_post | id + cons_date, data=match_data5, cluster = ~id+cons_date, weights = match_data5$weights)
etable(m1_all_energy_nomatch, m1_all_energy_match, m2_all_energy_match, m3_all_energy_match, m4_all_energy_match, m5_all_energy_match)


etable(m1_all_energy_nomatch, m1_all_energy_match, m3_all_energy_match, m4_all_energy_match,  tex=TRUE, file="../output_figures_tables/did_matching_results.tex", title = "Panel data analysis with matching\\label{tab:didmatch}", replace = TRUE)
