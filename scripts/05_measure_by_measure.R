# This script conducts a measure-by-measure analysis

# First we can see how many measures were implemented
dat %>%
  group_by(id) %>%
  summarise(across(contains("done"), mean)) %>%
  colSums(na.rm=T)
p2_mbm <- dat %>%
  group_by(id) %>%
  summarise(across(contains("done"), mean)) %>%
  dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
  colSums(na.rm=T)
p2_mbm <- tibble(measure=names(p2_mbm),number=p2_mbm) %>%
  mutate(term = gsub("\\_ugr_done|\\_upgrade_done","", measure))

# Remove the following types
# - ashp -- only one
# - gsph -- only two
# - oil_furnace -- not done
# - dhw -- not done
# - exp_floor -- v. few
# - type1ugr -- same as natural gas furnace upgrade

# Remane measures
measure_names <- tibble(
  term = c("air_sealing",
           "natural_gas_furnace",
           "ceiling_insulation",
           "windowsand_doors",
           "central_ac",
           "bsmt_insulation",
           "fnd_header",
           "walls_insulation"),
  full_name = c("air sealing",
                "gas furnace upgrade",
                "attic insulation",
                "window/door upgrade",
                "central ac upgrade",
                "basement insulation",
                "foundation header insulation",
                "wall insulation")
)


#####
# Create a correlation plot showing correlations between measure adoptions
res <- dat %>%
  group_by(id) %>%
  summarise(across(contains("done"), mean)) %>%
  dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
  drop_na() %>%
  rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
  cor()

png(file = "../output_figures_tables/correlation_plot.png",
    width = 6, # The width of the plot in inches
    height = 6,
    units = "in",
    res = 500) # The height of the plot in inches

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

dev.off()



#####
# Estimate regressions to determine energy savings by measure
rd_mbm <- rd %>%
  filter(ashp_upgrade_done == 0 | is.na(ashp_upgrade_done),
         gshp_upgrade_done == 0 | is.na(gshp_upgrade_done),
         exp_floor_ugr_done == 0 | is.na(exp_floor_ugr_done),
         oil_furnace_upgrade_done == 0 | is.na(oil_furnace_upgrade_done)) %>%
  # For all measures, replace NA (for non-participants) with 0
  mutate(across(contains("done"), ~replace_na(.,0))) %>%
  rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
  dplyr::select(-type1ugr_done, -exp_floor, -ashp, -gshp, -oil_furnace)

m2_gas_mbm <- feols(log(gas) ~ i(treated_post, air_sealing, ref=0) + 
                      i(treated_post, ceiling_insulation, ref=0) + 
                      i(treated_post, fnd_header, ref=0) + 
                      i(treated_post, windowsand_doors, ref=0) + 
                      i(treated_post, central_ac, ref=0)  + 
                      i(treated_post, bsmt_insulation, ref=0) + 
                      i(treated_post, walls_insulation, ref=0) + 
                      i(treated_post, natural_gas_furnace, ref=0)  | id + cons_date, data=rd_mbm, cluster = ~id+cons_date)

m2_elec_mbm <- feols(log(elec) ~ i(treated_post, air_sealing, ref=0) + 
                      i(treated_post, ceiling_insulation, ref=0) + 
                      i(treated_post, fnd_header, ref=0) + 
                      i(treated_post, windowsand_doors, ref=0) + 
                      i(treated_post, central_ac, ref=0)  + 
                      i(treated_post, bsmt_insulation, ref=0) + 
                      i(treated_post, walls_insulation, ref=0) + 
                      i(treated_post, natural_gas_furnace, ref=0)  | id + cons_date, data=rd_mbm, cluster = ~id+cons_date)

m2_energy_mbm <- feols(log(energy) ~ i(treated_post, air_sealing, ref=0) + 
                       i(treated_post, ceiling_insulation, ref=0) + 
                       i(treated_post, fnd_header, ref=0) + 
                       i(treated_post, windowsand_doors, ref=0) + 
                       i(treated_post, central_ac, ref=0)  + 
                       i(treated_post, bsmt_insulation, ref=0) + 
                       i(treated_post, walls_insulation, ref=0) + 
                       i(treated_post, natural_gas_furnace, ref=0)  | id + cons_date, data=rd_mbm, cluster = ~id+cons_date)

etable(m2_gas_mbm, m2_elec_mbm, m2_energy_mbm)
etable(m2_gas_mbm, m2_elec_mbm, m2_energy_mbm, tex=TRUE, file="../output_figures_tables/mbm_results_es.tex")

# Measure savings from all thermal envelope measures combined
deep_rf_estimated_gas <- glht(m2_gas_mbm, linfct = "`treated_post::TRUE:air_sealing` + 
           `treated_post::TRUE:ceiling_insulation` + 
           `treated_post::TRUE:fnd_header` + 
           `treated_post::TRUE:windowsand_doors` + 
           `treated_post::TRUE:bsmt_insulation` + 
           `treated_post::TRUE:walls_insulation`= 0")
deep_rf_estimated_elec <- glht(m2_elec_mbm, linfct = "`treated_post::TRUE:air_sealing` + 
           `treated_post::TRUE:ceiling_insulation` + 
           `treated_post::TRUE:fnd_header` + 
           `treated_post::TRUE:windowsand_doors` + 
           `treated_post::TRUE:bsmt_insulation` + 
           `treated_post::TRUE:walls_insulation`= 0")
deep_rf_estimated_energy <- glht(m2_energy_mbm, linfct = "`treated_post::TRUE:air_sealing` + 
           `treated_post::TRUE:ceiling_insulation` + 
           `treated_post::TRUE:fnd_header` + 
           `treated_post::TRUE:windowsand_doors` + 
           `treated_post::TRUE:bsmt_insulation` + 
           `treated_post::TRUE:walls_insulation`= 0")

dr_all <- tibble(
  term = "complete envelope retrofit",
  fuel = c("gas", "electricity", "energy"),
  estimate = c(coef(deep_rf_estimated_gas),
               coef(deep_rf_estimated_elec),
               coef(deep_rf_estimated_energy)),
  std.error = c(sqrt(vcov(deep_rf_estimated_gas)),
                sqrt(vcov(deep_rf_estimated_elec)),
                sqrt(vcov(deep_rf_estimated_energy))),
  number = NA
)


# Alternative approach to measure complete envelope retrofits
# Retain only houses that undergo complete retrofit + controls
complete_retrofit_sample <- rd %>%
  group_by(id) %>%
  summarise(across(contains("done"),  ~ mean(.x))) %>%
  mutate(complete_envelope_retrofit_ugr_done = (air_sealing_ugr_done &
                                            walls_insulation_ugr_done &
                                            (bsmt_insulation_ugr_done | fnd_header_ugr_done) &
                                            windowsand_doors_ugr_done )) %>%
  dplyr::select(id, complete_envelope_retrofit_ugr_done) %>%
  inner_join(rd) %>%
  mutate(across(contains("done"), ~replace_na(.,0))) %>%
  rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
  filter(treated == FALSE | complete_envelope_retrofit == TRUE)

m_cer_energy <- feols(log(energy) ~ i(treated_post, complete_envelope_retrofit, ref=0) + 
                         i(treated_post, natural_gas_furnace, ref=0)  | id + cons_date, data=complete_retrofit_sample, cluster = ~id+cons_date)
m_cer_gas <- feols(log(gas) ~ i(treated_post, complete_envelope_retrofit, ref=0) + 
                        i(treated_post, natural_gas_furnace, ref=0)  | id + cons_date, data=complete_retrofit_sample, cluster = ~id+cons_date)
m_cer_elec <- feols(log(elec) ~ i(treated_post, complete_envelope_retrofit, ref=0) + 
                        i(treated_post, natural_gas_furnace, ref=0)  | id + cons_date, data=complete_retrofit_sample, cluster = ~id+cons_date)

dr_all_actual <- tibble(
  term = "complete envelope retrofit",
  fuel = c("gas", "electricity", "energy"),
  estimate = c(coef(m_cer_gas)[1],
               coef(m_cer_elec)[1],
               coef(m_cer_energy)[1]),
  std.error = c(sqrt(diag(vcov(m_cer_gas)))[1],
                sqrt(diag(vcov(m_cer_elec)))[1],
                sqrt(diag(vcov(m_cer_energy)))[1]),
  number = complete_retrofit_sample %>% filter(complete_envelope_retrofit==TRUE) %>% dplyr::select(id) %>% n_distinct()
)

### Plot actual vs. imputed deep envelope retrofits
bind_rows(dr_all %>% 
             mutate(measure = "imputed"),
           dr_all_actual %>%
             mutate(measure = "actual")) %>%
  ggplot(aes(x=reorder(measure, number), y=estimate, colour=fuel)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate-1.96*std.error,
                    ymax=estimate+1.96*std.error)) +
  scale_colour_brewer(palette = "Set1", name=NULL) +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  labs(x=NULL,
       y="Estimated change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(legend.position = c(0.85,0.15)) +
  annotate(geom="rect",xmin=1.5,xmax=2.5,ymin=-Inf,ymax=Inf, fill="grey", alpha=0.5, colour=NA)

ggsave(filename="../output_figures_tables/cer_energy_savings.png", width=6, height=4) 

# Plot measure by measure coefficients
mbm_coefs <- 
  bind_rows(
    tidy(m2_gas_mbm) %>% mutate(fuel="gas"),
    tidy(m2_elec_mbm) %>% mutate(fuel="electricity"),
    tidy(m2_energy_mbm) %>% mutate(fuel="energy")
  ) %>%
  mutate(term = gsub("treated\\_post\\:\\:TRUE\\:","",term)) %>%
  inner_join(p2_mbm) %>%
  inner_join(measure_names) %>%
  dplyr::select(term=full_name, estimate, std.error, fuel, number) %>%
  bind_rows(dr_all)

p_mbm_es <- ggplot(mbm_coefs, aes(x=reorder(term, number), y=estimate, colour=fuel)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate-1.96*std.error,
                    ymax=estimate+1.96*std.error)) +
  scale_colour_brewer(palette = "Set1", name=NULL) +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  labs(x=NULL,
       y="Estimated change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(legend.position = c(0.85,0.55)) +
  annotate(geom="rect",xmin=8.5,xmax=9.5,ymin=-Inf,ymax=Inf, fill="grey", alpha=0.5, colour=NA)

ggsave(p_mbm_es, filename="../output_figures_tables/mbm_energy_savings.png", width=6, height=4)         

p_mbm_es_all_energy <- ggplot(mbm_coefs %>% filter(fuel == "energy"), aes(x=reorder(term, number), y=estimate)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate-1.96*std.error,
                    ymax=estimate+1.96*std.error)) +
  scale_colour_brewer(palette = "Set1", name=NULL) +
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0) +
  labs(x=NULL,
       y="Estimated change in energy consumption") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(legend.position = c(0.85,0.3)) +
  annotate(geom="rect",xmin=8.5,xmax=9.5,ymin=-Inf,ymax=Inf, fill="grey", alpha=0.5, colour=NA)

ggsave(p_mbm_es_all_energy, filename="../output_figures_tables/mbm_energy_savings_energy_only.png", width=6, height=4) 

p_mbm_count <- mbm_coefs %>%
  group_by(term) %>%
  summarise(number=mean(number),
            estimate=mean(estimate)) %>%
  ggplot(aes(x=reorder(term, number), y=number)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  labs(x=NULL,
       y="Number") +
  theme(axis.text.y=element_blank()) +
  annotate(geom="rect",xmin=8.5,xmax=9.5,ymin=-Inf,ymax=Inf, fill="grey", alpha=0.5, colour=NA)

p_mbm_es + p_mbm_count + plot_layout(ncol=2, nrow=1, widths =c(2,1))

p_w_mbm <- p_mbm_es + p_mbm_count + plot_layout(ncol=2, nrow=1, widths =c(2,1))
ggsave(p_w_mbm, filename ="../output_figures_tables/mbm_energy_savings_combined.png", width=8, height=6)         

p_mbm_count_all_energy <- mbm_coefs %>%
  filter(fuel == "energy") %>%
  group_by(term) %>%
  summarise(number=mean(number),
            estimate=mean(estimate)) %>%
  ggplot(aes(x=reorder(term, number), y=number)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  labs(x=NULL,
       y="Number") +
  theme(axis.text.y=element_blank()) +
  annotate(geom="rect",xmin=8.5,xmax=9.5,ymin=-Inf,ymax=Inf, fill="grey", alpha=0.5, colour=NA)

p_mbm_es_all_energy + p_mbm_count_all_energy + plot_layout(ncol=2, nrow=1, widths =c(2,1))

p_w_mbm_all_energy <- p_mbm_es_all_energy + p_mbm_count_all_energy + plot_layout(ncol=2, nrow=1, widths =c(2,1))
ggsave(p_w_mbm_all_energy, filename ="../output_figures_tables/mbm_energy_savings_combined_all_energy.png", width=8, height=6)         
