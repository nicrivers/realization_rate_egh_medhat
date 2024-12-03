# Graph the event study

# In levels rather than logs

##################
# TWFE estimators
##################

# TWFE estimator  | All households
es_twfe_all <- feols(energy ~ i(years_to_treatment, ref = c(-1, -1000)) | id + consyear, rd_stag %>% 
                       filter(years_to_treatment != -2000))

# TWFE estimator  | Participants only
es_twfe_partic = feols(energy ~ i(years_to_treatment, ref = c(-1, -1000)) | id + consyear, rd_stag %>% 
                         filter(treated == TRUE, 
                                years_to_treatment != -2000,
                                consyear < 2012))

############################
# Sun and Abraham estimators
############################

# SA estimator    | All households
es_sunab_all <- feols(energy ~ sunab(retrofit_start_year, consyear) | id + consyear, rd_stag %>% 
                        filter(years_to_treatment != -2000) %>% 
                        # Set treatment year to the future for untreated households
                        replace_na(list(retrofit_start_year=3000)) , cluster=~id + consyear)

# SA estimator    | Participants only
es_sunab_partic <- feols(energy ~ sunab(retrofit_start_year, consyear) | id + consyear, rd_stag %>% 
                           filter(treated == TRUE,
                                  years_to_treatment != -2000,
                                  consyear < 2012), cluster=~id + consyear)

###################################
# Callaway and Sant'anna estimators
###################################

# Set up the data
rd_stag_cs <- rd_stag %>%
  ungroup() %>%
  # Create the dependent variable, and remove missing
  mutate(log_energy = log(energy)) %>%
  filter(!is.na(log_energy),
         log_energy != -Inf) %>%
  # For observations that are never treated, set retrofit start year to zero
  # As in https://bcallaway11.github.io/did/articles/did-basics.html#an-example-with-real-data
  # I can also set to a large number (e.g., 3000), as in https://raw.githack.com/Mixtape-Sessions/Advanced-DID/main/Exercises/Exercise-1/Solutions/medicaid-analysis-solutions-R.html
  # and this generates identical results
  mutate(retrofit_start_year = 
           if_else(is.na(retrofit_start_year), 0, retrofit_start_year)) %>%
  # Remove years between first and last audit
  filter(years_to_treatment != -2000) %>%
  dplyr::select(id, consyear, retrofit_start_year, energy)


m_cs_all <- att_gt(
  yname = "energy",
  tname = "consyear",
  idname = "id",
  gname = "retrofit_start_year",
  control_group = "notyettreated",
  xformla = ~1,
  data = rd_stag_cs,
  allow_unbalanced_panel = TRUE,
  base_period = "universal"
)

es_cs_all <- aggte(m_cs_all, type="dynamic", na.rm=T)

m_cs_partic <- att_gt(yname = "energy",
                      tname = "consyear",
                      idname = "id",
                      gname = "retrofit_start_year",
                      xformla = ~1,
                      control_group = "notyettreated",
                      data = rd_stag_cs %>%
                        filter(retrofit_start_year > 0),
                      allow_unbalanced_panel = TRUE,
                      base_period = "universal"
)

es_cs_partic <- aggte(m_cs_partic, type="dynamic", na.rm=T)

##########################
# Collect all coefficients
##########################

cs_df <- tibble(
  term = es_cs_all$egt,
  estimate = es_cs_all$att.egt,
  std.error = es_cs_all$se.egt,
  model="Callaway&Sant'Anna",
  sample="All"
) %>%
  bind_rows(
    tibble(
      term = es_cs_partic$egt,
      estimate = es_cs_partic$att.egt,
      std.error = es_cs_partic$se.egt,
      model="Callaway&Sant'Anna",
      sample="Participants"
    )
  )

# Collect all coefficients
all_es_coefs <-
  bind_rows(
    tidy(es_twfe_all) %>% mutate(model = "TWFE", sample="All"),
    tidy(es_twfe_partic) %>% mutate(model = "TWFE", sample="Participants"),
    tidy(es_sunab_all) %>% mutate(model="Sun&Abraham", sample="All"),
    tidy(es_sunab_partic) %>% mutate(model="Sun&Abraham", sample="Participants")
  ) %>%
  dplyr::select(
    term, estimate, std.error, model, sample
  ) %>%
  mutate(term = str_extract(term,"-?\\d+"),
         term = as.numeric(term)) %>%
  bind_rows(cs_df) %>%
  # Keep five years before and 10 years after treatment
  filter(term <= 10, term >= -5) %>%
  # Manually impose zero point estimate in year before treatment where missing
  bind_rows(
    expand_grid(model=c("Callaway&Sant'Anna","Sun&Abraham","TWFE"), sample=c("All", "Participants"), term=-1, estimate=0, std.error=NA)
  )


# Plot the event study
ggplot(all_es_coefs %>% mutate(estimate=estimate*12, std.error=std.error*12) %>% filter(sample=="All"), aes(x=term, y=estimate, colour=model)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate-1.96*std.error,
                    ymax=estimate+1.96*std.error)) +
  scale_colour_brewer(palette="Set1", name=NULL) +
  theme_bw() +
  theme(legend.position = c(0.2,0.2)) +
  labs(x="Years until treatment",
       y="Impact on total energy consumption (GJ/year)") 

ggsave("../output_figures_tables/event_study_plot_levels.png", width=6, height=6)


