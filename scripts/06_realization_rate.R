# This script estimates realization rates for overall and measure-specific energy savings investments

#####
# Projected Energy savings by measure
# First, regress projected savings on actual measures completed to get an imputed measure
# of projected savings for each measure

# This is a cross sectional data set that includes one observation for each household in the program
rd_ps <- rd %>%
  filter(treated == TRUE) %>%
  group_by(id) %>%
  summarise(across(everything(), mean)) %>%
  rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
  filter(exp_floor != 1,
         ashp != 1,
         oil_furnace != 1,
         dhw != 1) %>% 
  # Some (4) fnd_headers get coded as NA rather than 0
  replace_na(list(fnd_header=0))

# Regress projected savings on measures completed
projected_mbm_gas <- feols(log(postretrofit_naturalgasconsum) - log(preretrofit_naturalgasconsum) ~ 
                     air_sealing + 
                     ceiling_insulation +  
                     fnd_header + 
                     windowsand_doors + 
                     central_ac  + 
                     bsmt_insulation +  
                     walls_insulation + 
                     natural_gas_furnace - 1, data=rd_ps)

projected_mbm_elec <- feols(log(postretrofit_electricalconsump) - log(preretrofit_electicalconsumpti) ~ 
                             air_sealing + 
                             ceiling_insulation +  
                             fnd_header + 
                             windowsand_doors + 
                             central_ac  + 
                             bsmt_insulation +  
                             walls_insulation + 
                             natural_gas_furnace - 1, data=rd_ps)


projected_mbm_energy <- feols(log(postretrofit_energy) - log(preretrofit_energy) ~ 
                              air_sealing + 
                              ceiling_insulation +  
                              fnd_header + 
                              windowsand_doors + 
                              central_ac  + 
                              bsmt_insulation +  
                              walls_insulation + 
                              natural_gas_furnace - 1, data=rd_ps)

etable(projected_mbm_gas, projected_mbm_elec, projected_mbm_energy)
etable(projected_mbm_gas, projected_mbm_elec, projected_mbm_energy, tex=TRUE, file = "../output_figures_tables/projected_es_mbm.tex", replace = TRUE)

# Plot projected savings coefficients
mbm_projected_coefs <- 
  bind_rows(
    tidy(projected_mbm_gas) %>% mutate(fuel = "gas"),
    tidy(projected_mbm_elec) %>% mutate(fuel = "electricity"),
    tidy(projected_mbm_energy) %>% mutate(fuel = "energy")
  ) %>%
  inner_join(p2_mbm)

ggplot(mbm_projected_coefs, aes(x=reorder(term, number), y=estimate, colour=fuel)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate - 1.96*std.error,
                    ymax=estimate + 1.96*std.error)) +
  geom_hline(yintercept = 0) +
  labs(x=NULL,
       y="Projected change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format()) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(0.2,0.6)) +
  scale_colour_brewer(name=NULL, palette = "Set1")

ggsave("../output_figures_tables/projected_es_mbm.png", width = 6, height=4)

        

## energy only
p_mbm_projected <- ggplot(mbm_projected_coefs %>% filter(fuel == "energy"), aes(x=reorder(term, number), y=estimate)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate - 1.96*std.error,
                    ymax=estimate + 1.96*std.error)) +
  geom_hline(yintercept = 0) +
  labs(x=NULL,
       y="Projected change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format()) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = c(0.2,0.8)) +
  scale_colour_brewer(name=NULL, palette = "Set1")

ggsave(p_mbm_projected, "../output_figures_tables/projected_es_mbm_all_energy.png", width = 6, height=4)

# Plot with number chart on right

p_w_mbm_projected <- p_mbm_projected + p_mbm_count_all_energy + plot_layout(ncol=2, nrow=1, widths =c(2,1))
ggsave(p_w_mbm_all_energy, filename ="../output_figures_tables/mbm_w_projeted_combined_all_energy.png", width=8, height=6) 

##### 
# Realization rate -- aggregate retrofits
rd_rr_mbm <- rd %>%
  mutate(delta_gas = case_when(
    treated == TRUE ~ log(postretrofit_naturalgasconsum) - log(preretrofit_naturalgasconsum),
    treated == FALSE ~ 0),
    delta_elec = case_when(
      treated == TRUE ~ log(postretrofit_electricalconsump) - log(preretrofit_electicalconsumpti),
      treated == FALSE ~ 0),
    delta_energy = case_when(
      treated == TRUE ~ log(postretrofit_energy) - log(preretrofit_energy),
      treated == FALSE ~ 0
    )) %>%
  filter(ashp_upgrade_done == 0 | is.na(ashp_upgrade_done),
         gshp_upgrade_done == 0 | is.na(gshp_upgrade_done),
         exp_floor_ugr_done == 0 | is.na(exp_floor_ugr_done),
         oil_furnace_upgrade_done == 0 | is.na(oil_furnace_upgrade_done))

m1_gas_rr <- feols(log(gas) ~ as.numeric(treated_post) : delta_gas  | id + cons_date, data=rd_rr_mbm, cluster = ~id+cons_date)
m1_elec_rr <- feols(log(elec) ~ as.numeric(treated_post) : delta_elec  | id + cons_date, data=rd_rr_mbm, cluster = ~id+cons_date)
m1_energy_rr <- feols(log(energy) ~ as.numeric(treated_post) : delta_energy  | id + cons_date, data=rd_rr_mbm, cluster = ~id+cons_date)
etable(m1_gas_rr, m1_elec_rr, m1_energy_rr)
etable(m1_gas_rr, m1_elec_rr, m1_energy_rr, tex=TRUE, file="../output_figures_tables/overall_realizationrate_log.tex", replace = TRUE)


# Re-estimate realization rate in levels rather than logs
rd_rr_mbm_levels <- rd_rr_mbm %>%
  mutate(delta_gas_lev = case_when(
    treated == TRUE ~ postretrofit_naturalgasconsum - preretrofit_naturalgasconsum,
    treated == FALSE ~ 0),
    delta_elec_lev = case_when(
      treated == TRUE ~ postretrofit_electricalconsump - preretrofit_electicalconsumpti,
      treated == FALSE ~ 0),
    delta_energy_lev = case_when(
      treated == TRUE ~ postretrofit_energy - preretrofit_energy,
      treated == FALSE ~ 0),
    elec = elec * 12,
    gas = gas * 12 * 35.301,
    energy = energy * 12
    )

m1_gas_rr_lev <- feols(gas ~ as.numeric(treated_post) : delta_gas_lev  | id + cons_date, data=rd_rr_mbm_levels, cluster = ~id+cons_date)
m1_elec_rr_lev <- feols(elec ~ as.numeric(treated_post) : delta_elec_lev  | id + cons_date, data=rd_rr_mbm_levels, cluster = ~id+cons_date)
m1_energy_rr_lev <- feols(energy ~ as.numeric(treated_post) : delta_energy_lev  | id + cons_date, data=rd_rr_mbm_levels, cluster = ~id+cons_date)
etable(m1_gas_rr_lev, m1_elec_rr_lev, m1_energy_rr_lev)  
etable(m1_gas_rr_lev, m1_elec_rr_lev, m1_energy_rr_lev, tex=TRUE, file="../output_figures_tables/overall_realizationrate_levels.tex", replace = TRUE)


#####
# Realization rates -- measure by measure
# Compare projected savings to actual savings

projected_vs_realized <-
  inner_join(
    mbm_coefs %>% dplyr::select(term, fuel, estimated_savings=estimate, estimated_savings_se=std.error, number),
    mbm_projected_coefs %>% dplyr::select(term, fuel, projected_savings=estimate, projected_savings_se=std.error)
  ) %>%
  mutate(realization_rate = estimated_savings/projected_savings,
         rr_label=case_when(
           realization_rate < 0 ~ "NA",
           realization_rate >=0 & projected_savings < 0 & realization_rate < 2 ~ paste0(round(realization_rate*100,0),"%"),
           projected_savings >= 0 ~ "NA",
           realization_rate > 2 ~ "> 200%"
           )
         )

ggplot(projected_vs_realized,
       aes(x=reorder(term, estimated_savings))) +
  geom_point(aes(y=estimated_savings,colour="Estimated")) +
  geom_point(aes(y=projected_savings, colour="Projected")) +
  facet_wrap(~ factor(fuel,levels = c("gas","electricity","energy"))) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=estimated_savings-1.96*estimated_savings_se,
                    ymax=estimated_savings+1.96*estimated_savings_se,
                    colour="Estimated"),
                width=0.25) +
  geom_errorbar(aes(ymin=projected_savings-1.96*projected_savings_se,
                    ymax=projected_savings+1.96*projected_savings_se,
                    colour="Projected"),
                width=0.25) +
  labs(x=NULL,
       y="Change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme_bw() +
  scale_colour_brewer(name=NULL, palette="Set1") +
  geom_text(aes(y=estimated_savings, label=rr_label), size=3, nudge_x = 0.25)

ggsave("../output_figures_tables/mbm_realization_rate.png", width=8, height=6)

## Energy only
ggplot(projected_vs_realized %>% filter(fuel == "energy"),
       aes(x=reorder(term, number))) +
  geom_point(aes(y=estimated_savings,colour="Estimated")) +
  geom_point(aes(y=projected_savings, colour="Projected")) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin=estimated_savings-1.96*estimated_savings_se,
                    ymax=estimated_savings+1.96*estimated_savings_se,
                    colour="Estimated"),
                width=0.25) +
  geom_errorbar(aes(ymin=projected_savings-1.96*projected_savings_se,
                    ymax=projected_savings+1.96*projected_savings_se,
                    colour="Projected"),
                width=0.25) +
  labs(x=NULL,
       y="Change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme_bw() +
  scale_colour_brewer(name=NULL, palette="Set1") +
  geom_text(aes(y=estimated_savings, label=rr_label), size=3, nudge_x = 0.25)

ggsave("../output_figures_tables/mbm_realization_rate_all_energy.png", width=8, height=6)

