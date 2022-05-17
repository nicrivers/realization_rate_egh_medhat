# Graph the event study

# I work with annual data for the event study (because of package errors with monthly data)
rd_stag <- rd %>%
  mutate(retrofit_start_year = year(preretrofit_entrydate),
         retrofit_end_year = year(postretrofit_entrydate),
         years_to_treatment = case_when(
           consyear < retrofit_start_year ~ consyear - retrofit_start_year,
           consyear > retrofit_end_year ~ consyear - retrofit_end_year,
           is.na(retrofit_end_year) ~ -1000,
           consyear >= retrofit_start_year & consyear <= retrofit_end_year ~ -2000
         )) %>%
  group_by(id,consyear,treated,post,treated_post,years_to_treatment,retrofit_start_year, retrofit_end_year) %>%
  summarise(elec=sum(elec, na.rm=T),
            gas=sum(gas, na.rm=T),
            energy=sum(energy, na.rm=T))


res_twfe_onlytreated = feols(log(energy) ~ i(years_to_treatment, ref = c(-1, -1000)) | id + consyear, rd_stag %>% 
                               filter(treated == TRUE, 
                                      years_to_treatment != -2000))

res_twfe = feols(log(energy) ~ i(years_to_treatment, ref = c(-1, -1000)) | id + consyear, rd_stag %>% 
                   filter(years_to_treatment != -2000))


# Sun and Abraham package
res_sunab = feols(log(energy) ~ sunab(retrofit_end_year, consyear) | id + consyear, rd_stag %>% 
                    filter(years_to_treatment != -2000) %>% 
                    mutate(retrofit_end_year = if_else(treated == FALSE, 10000, retrofit_end_year)))

# Callaway an Sant'a Anna package
# In this case, I didn't drop the period *during* the retrofit because of package errors. 
# This affects coefficient estimates for -1 and 0 periods.
rd_stag_cs <- rd %>%
  mutate(retrofit_start_year = year(preretrofit_entrydate),
         retrofit_end_year = year(postretrofit_entrydate)) %>%
  group_by(id,consyear,treated,retrofit_start_year, retrofit_end_year) %>%
  summarise(elec=sum(elec, na.rm=T),
            gas=sum(gas, na.rm=T),
            energy=sum(energy, na.rm=T)) %>%
  mutate(retrofit_end_year = if_else(is.na(retrofit_end_year),0,retrofit_end_year),
         log_gas=log(gas),
         log_elec=log(elec),
         log_energy=log(energy)) %>%
  filter(!is.na(log_gas),
         !is.na(log_elec),
         !is.na(log_energy),
         log_gas != -Inf,
         log_elec != -Inf,
         log_energy != -Inf)

res_cs <- att_gt(yname = "log_gas",
                 gname = "retrofit_end_year",
                 idname = "id",
                 tname = "consyear",
                 xformla = ~1,
                 data = rd_stag_cs
)

cs_coefs <- aggte(res_cs, type="dynamic")

cs_df <- tibble(
  term = cs_coefs$egt,
  estimate = cs_coefs$att.egt,
  std.error = cs_coefs$se.egt,
  model="Callaway&Sant'aAnna"
)

# Collect all coefficients
all_es_coefs <-
  bind_rows(
  tidy(res_sunab) %>% mutate(model = "Sun&Abraham"),
  tidy(res_twfe_onlytreated) %>% mutate(model = "TWFE-only treated"),
  tidy(res_twfe) %>% mutate(model="TWFE-incl. never treated")
  ) %>%
  dplyr::select(
    term, estimate, std.error, model
  ) %>%
  mutate(term = str_extract(term,"-?\\d+"),
         term = as.numeric(term)) %>%
  bind_rows(cs_df)

ggplot(all_es_coefs, aes(x=term, y=estimate, colour=model)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(position=position_dodge(width=0.5),
                aes(ymin=estimate-1.96*std.error,
                    ymax=estimate+1.96*std.error)) +
  scale_colour_brewer(palette="Set1", name=NULL) +
  theme_bw() +
  theme(legend.position = c(0.2,0.2)) +
  labs(x="Years until treatment",
       y="Impact on total energy consumption") +
  scale_y_continuous(labels=scales::percent_format())

ggsave("../output_figures_tables/event_study_plot.png", width=6, height=4)
