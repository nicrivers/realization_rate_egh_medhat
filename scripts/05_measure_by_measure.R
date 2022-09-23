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


#####
# Create a correlation plot showing correlations between measure adoptions
res <- dat %>%
  group_by(id) %>%
  summarise(across(contains("done"), mean)) %>%
  select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
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
  select(-type1ugr_done, -exp_floor, -ashp, -gshp, -oil_furnace)

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

# Plot measure by measure coefficients
mbm_coefs <- 
  bind_rows(
    tidy(m2_gas_mbm) %>% mutate(fuel="gas"),
    tidy(m2_elec_mbm) %>% mutate(fuel="electricity"),
    tidy(m2_energy_mbm) %>% mutate(fuel="energy")
  ) %>%
  mutate(term = gsub("treated\\_post\\:\\:TRUE\\:","",term)) %>%
  inner_join(p2_mbm)

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
  theme(legend.position = c(0.85,0.75)) 

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
  theme(legend.position = c(0.85,0.3)) 

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
  theme(axis.text.y=element_blank())

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
  theme(axis.text.y=element_blank())

p_mbm_es_all_energy + p_mbm_count_all_energy + plot_layout(ncol=2, nrow=1, widths =c(2,1))

p_w_mbm_all_energy <- p_mbm_es_all_energy + p_mbm_count_all_energy + plot_layout(ncol=2, nrow=1, widths =c(2,1))
ggsave(p_w_mbm_all_energy, filename ="../output_figures_tables/mbm_energy_savings_combined_all_energy.png", width=8, height=6)         
