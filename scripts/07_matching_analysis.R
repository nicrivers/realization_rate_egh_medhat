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

balance_unmatched <- matchit(treated ~ gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                             data=pre_treat, method=NULL, distance="glm")

summary(balance_unmatched)
# Treated houses consume more gas

# 1:1 propensity score matching
m.out1 <- matchit(treated ~ gas_shoulder + gas_summer + gas_winter + elec_shoulder + elec_summer + elec_winter, 
                  data=pre_treat, method="nearest", distance="glm")

# much closer balance
summary(m.out1, un=FALSE)

# see matching
plot(m.out1, type = "jitter", interactive = FALSE)
plot(m.out1, type = "qq", interactive = FALSE, which.xs = c("gas_winter", "elec_summer"))

# Now we can estimate treatment effect on this matched sample
match_data <- match.data(m.out1) %>%
  dplyr::select(id, weights) %>%
  inner_join(rd)

m1_all_gas_match <- feols(log(gas) ~ treated_post | id + cons_date, data=match_data, cluster = ~id+cons_date, weights = match_data$weights)
m1_all_elec_match <- feols(log(elec) ~ treated_post | id + cons_date , data=match_data, cluster = ~id+cons_date, weights = match_data$weights)
etable(m1_all_gas_match, m1_all_elec_match)

etable(m1_all_gas_match, m1_all_elec_match,  tex=TRUE, file="../output_figures_tables/did_matching_results.tex")
