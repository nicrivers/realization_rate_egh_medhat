# Use annual data
rd_year <- rd %>%
  group_by(id,year) %>%
  summarise(energy=sum(energy, na.rm=T),
            treated_post = mean(treated_post),
            across(contains("done"), mean)) %>%
  mutate(across(contains("done"), ~replace_na(.,0))) %>%
  # Only keep when treatment status is clear
  filter(treated_post == 0 | treated_post == 1) %>%
  rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
  dplyr::select(-type1ugr_done, -exp_floor, -ashp, -gshp, -oil_furnace)

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
  # Projected vs. realized bill savings
  # Assume all savings gas at $5.98/GJ (average pre-retrofit price)
  mutate(dollar_bill_saving_projected = predicted_savings * 5.98,
         dollar_bill_saving_realized = savings * 5.98) %>%
  dplyr::select(average_rebate, contains("dollar"))

print(xtable::xtable(reb), "../output_figures_tables/dollar_savings.tex", type="latex")


####################
# Measure by measure
####################

measure_names_inc <- tibble(
  measure_name = c("air_seal",
           "gas_furn",
           "attic_1",
           "attic_2",
           "attic",
           "basement",
           "header",
           "wind",
           "doors",
           "wind_door",
           "walls",
           "walls_1",
           "walls_2",
           "walls_3",
           "walls_4",
           "walls_5"),
  measure = c("air_sealing",
                "natural_gas_furnace",
                "ceiling_insulation",
              "ceiling_insulation",
              "ceiling_insulation",
              "bsmt_insulation",
              "fnd_header",
                "windowsand_doors",
                "windowsand_doors",
              "windowsand_doors",
                "walls_insulation",
              "walls_insulation",
              "walls_insulation",
              "walls_insulation",
              "walls_insulation",
              "walls_insulation")
)

### Note that there a no walls or basement incentives recorded
reb <- rd %>%
  filter(treated == TRUE) %>%
  group_by(id) %>%
  summarise(across(contains("paid"), mean)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  summarise(across(everything(), \(x) mean(x, na.rm=T))) %>%
  rename_with(., ~gsub("\\_paid_inc","", .x))

reb <- tibble(measure_name=colnames(reb),average_rebate=as_vector(reb)) %>% 
  inner_join(measure_names_inc) %>%
  group_by(measure) %>%
  summarise(average_rebate = sum(average_rebate))

# Regression with energy in levels (GJ)
m1_mbm <- feols(energy ~ i(treated_post, air_sealing, ref=0) + 
                         i(treated_post, ceiling_insulation, ref=0) + 
                         i(treated_post, fnd_header, ref=0) + 
                         i(treated_post, windowsand_doors, ref=0) + 
                         i(treated_post, central_ac, ref=0)  + 
                         i(treated_post, bsmt_insulation, ref=0) + 
                         i(treated_post, walls_insulation, ref=0) + 
                         i(treated_post, natural_gas_furnace, ref=0) | id + year , data=rd_year, cluster = ~id+year)

projected_mbm_energy <- feols( (postretrofit_energy - preretrofit_energy) ~ 
                                air_sealing + 
                                ceiling_insulation +  
                                fnd_header + 
                                windowsand_doors + 
                                central_ac  + 
                                bsmt_insulation +  
                                walls_insulation + 
                                natural_gas_furnace - 1, data=rd_ps)

realized_savings <- tidy(m1_mbm) %>%
  # Only keep estimate sig dif than zero
  filter(p.value < 0.05) %>%
  dplyr::select(measure = term, 
                savings = estimate) %>%
  mutate(measure = gsub("treated_post::1:","",measure)) %>%
  mutate(savings = - savings)

projected_savings <- tidy(projected_mbm_energy) %>%
  # Only keep estimate sig dif than zero
  filter(p.value < 0.05) %>%
  dplyr::select(measure=term,
                projected_savings = estimate) %>%
  mutate(projected_savings = - projected_savings)

mbm_reb <- reb %>%
  left_join(realized_savings, ) %>%
  inner_join(projected_savings) %>%
  mutate(dollar_per_annual_gj_realized = average_rebate / savings,
         dollar_per_annual_gj_projected = average_rebate / projected_savings) %>%
  # Projected vs. realized bill savings
  # Assume all savings gas at $5.98/GJ (average pre-retrofit price)
  mutate(dollar_bill_saving_projected = projected_savings * 5.98,
         dollar_bill_saving_realized = savings * 5.98)

  
print(xtable::xtable(mbm_reb), "../output_figures_tables/dollar_savings_mbm.tex", type="latex")
