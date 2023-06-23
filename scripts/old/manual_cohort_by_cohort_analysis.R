# This website is excellent:
# https://rpubs.com/corinne-riddell/guide-to-did-estimators

# How many households retrofit each year?
rd_stag %>%
  group_by(id) %>%
  summarise(retrofit_end_year = mean(retrofit_end_year)) %>%
  filter(!is.na(retrofit_end_year)) %>%
  group_by(retrofit_end_year) %>%
  summarise(n = n())

#   retrofit_end_year     n
#               <dbl> <int>
#1              2008    24
#2              2009   309
#3              2010   532
#4              2011   360
#5              2012   228

# Treatment effects for the 2008 cohort

rd_2008 <- rd_stag %>%
  # Participants only
  filter(treated == TRUE) %>%
  # Keep 2008 and other households pre-retrofit
  filter(retrofit_end_year == 2008 |
           consyear < retrofit_start_year) %>%
  # Remove years between retrofit start and end
  filter(years_to_treatment != -2000)

# Run the regression
m2008 <- feols(log(energy) ~ treated_post | id + consyear, data=rd_2008)
# With dynamics
m2008d <- feols(log(energy) ~ i(years_to_treatment, ref=c(-5, -1)) | id + consyear, data=rd_2008)

# Treatment effects for the 2009 cohort

rd_2009 <- rd_stag %>%
  # Participants only
  filter(treated == TRUE) %>%
  # Keep 2009 and other households pre-retrofit
  filter(retrofit_end_year == 2009 |
           consyear < retrofit_start_year) %>%
  # Remove years between retrofit start and end
  filter(years_to_treatment != -2000)
          

# Run the regression
m2009 <- feols(log(energy) ~ treated_post | id + consyear, data=rd_2009)
# With dynamics
m2009d <- feols(log(energy) ~ i(years_to_treatment, ref=c(-5, -1)) | id + consyear, data=rd_2009)

# Treatment effects for the 2010 cohort
rd_2010 <- rd_stag %>%
  # Participants only
  filter(treated == TRUE) %>%
  # Keep 2009 and other households pre-retrofit
  filter(retrofit_end_year == 2010 |
           consyear < retrofit_start_year) %>%
  # Remove years between retrofit start and end
  filter(years_to_treatment != -2000)


# Run the regression
m2010 <- feols(log(energy) ~ treated_post | id + consyear, data=rd_2010)
# With dynamics
m2010d <- feols(log(energy) ~ i(years_to_treatment, ref=c(-5, -1)) | id + consyear, data=rd_2010)

# Treatment effects for the 2011 cohort
rd_2011 <- rd_stag %>%
  # Participants only
  filter(treated == TRUE) %>%
  # Keep 2009 and other households pre-retrofit
  filter(retrofit_end_year == 2011 |
           consyear < retrofit_start_year) %>%
  # Remove years between retrofit start and end
  filter(years_to_treatment != -2000)


# Run the regression
m2011 <- feols(log(energy) ~ treated_post | id + consyear, data=rd_2011)
# With dynamics
m2011d <- feols(log(energy) ~ i(years_to_treatment, ref=c(-5, -1)) | id + consyear, data=rd_2011)

# Treatment effects for the 2012 cohort
rd_2012 <- rd_stag %>%
  # Participants only
  filter(treated == TRUE) %>%
  # Keep 2009 and other households pre-retrofit
  filter(retrofit_end_year == 2012 |
           consyear < retrofit_start_year) %>%
  # Remove years between retrofit start and end
  filter(years_to_treatment != -2000)


# Run the regression
m2012 <- feols(log(energy) ~ treated_post | id + consyear, data=rd_2012)
# With dynamics
m2012d <- feols(log(energy) ~ i(years_to_treatment, ref=c(-5, -1)) | id + consyear, data=rd_2012)

# Aggregate results
etable(m2008, m2009, m2010)

# Aggregate dynamic results
etable(m2008d, m2009d, m2010d)
etable(m2008d, m2009d, m2010d, tex = TRUE, file = "../output_figures_tables/dynamic_cohort_estimates.tex")

# Try to replicate with Sun and Abraham estimator
# using fixest
m_sa <- feols(log(energy) ~ sunab(retrofit_end_year, consyear, ref.p=c(.F, -2)) | id + consyear, data=rd_stag %>% filter(treated == TRUE, years_to_treatment != -2000))

# calculate weights using bacondecomp
library(bacondecomp)
bacon(energy ~ treated_post, data=rd_stag, id_var = "id", time_var = "consyear")
# unbalanced panel
rd_stag %>%
  filter(treated == TRUE,
         !is.na(energy)) %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  filter(n == 13) %>%
  inner_join(rd_stag) %>% 
  filter(!is.na(energy)) %>%
  dplyr::select(treated_post, consyear, id, energy) %>%
  mutate(treated_post = as.numeric(treated_post),
         energy = log(energy)) -> rd_stag_bal

bacon(energy ~ treated_post, data=rd_stag_bal, id_var = "id", time_var = "consyear")
# Most of the weight (70%) is on later vs. earlier treated. This is a problem!
# treatment effects also looks small: about 5-7%

# Try the callaway and sant'anna estimator
cs <- att_gt(yname = "log_energy",
             tname = "consyear",
             idname = "id",
             gname = "retrofit_end_year",
             data = rd_stag %>% mutate(log_energy = log(energy)),
             control_group = "notyettreated",
             anticipation = 0)

cs_agg <- aggte(cs, type="dynamic")
aggte(cs, type="simple")
# This generates sensible coefficient estimates
# Note that there are some reductions before treatment because retrofits take place over time
# I'm not sure why there's an increase in the pre-period.
ggdid(cs_agg)

# Making Goodman-Bacon's four plots
# Contrast 1: Treated vs. never treated
# 2008
treatment_year = 2008
cp <- function(ty) {
newdat <- rd_stag %>%
  filter(retrofit_end_year == ty | treated == FALSE) %>%
  group_by(treated, consyear) %>%
  summarise(energy = mean(energy, na.rm=T)) 

did_coef <- feols(log(energy) ~ treated * (consyear > ty) | treated + consyear , data=newdat)  
  
newdat %>%
  ggplot(aes(x=consyear, y=energy, colour=treated)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = ty, linetype="dashed") +
  theme_bw() +
  labs(x=NULL,
       y="Energy (GJ)",
       title=paste("Treated in ", ty, " vs never treated")) +
  annotate(geom="label", x=ty+1, y=mean(newdat$energy), label=paste("DiD: ", round(did_coef$coeftable$Estimate*100, 0), "%"))
}

cp(2008)
ggsave("../output_figures_tables/cp2008.png", width=6, height=4)
cp(2009)
ggsave("../output_figures_tables/cp2009.png", width=6, height=4)
cp(2010)
ggsave("../output_figures_tables/cp2010.png", width=6, height=4)
cp(2011)
ggsave("../output_figures_tables/cp2011.png", width=6, height=4)
cp(2012)
ggsave("../output_figures_tables/cp2012.png", width=6, height=4)

# Contrast 2: Earlier vs later treated
el <- function(ty) {
newdat <- rd_stag %>%
    filter(treated == TRUE) %>%
    filter(retrofit_end_year == ty | consyear < retrofit_end_year) %>%
    mutate(treated = (retrofit_end_year == ty)) %>%
    filter(consyear < 2012) %>%
    group_by(treated, consyear) %>%
    summarise(energy = mean(energy, na.rm=T))

did_coef <- feols(log(energy) ~ treated * (consyear > ty) | treated + consyear , data=newdat)  

newdat %>%
  ggplot(aes(x=consyear, y=energy, colour=treated)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = ty, linetype="dashed") +
    theme_bw() +
    labs(x=NULL,
         y="Energy (GJ)",
         title=paste("Treated in ", ty, " vs later treated")) +
  annotate(geom="label", x=ty+1, y=mean(newdat$energy), label=paste("DiD: ", round(did_coef$coeftable$Estimate*100, 0), "%"))
}

el(2008)
ggsave("../output_figures_tables/el2008.png", width=6, height=4)
el(2009)
ggsave("../output_figures_tables/el2009.png", width=6, height=4)
el(2010)
ggsave("../output_figures_tables/el2010.png", width=6, height=4)

# Contrast 3: Later vs earlier treated (problem!)
le <- function(ty) {
newdat <-  rd_stag %>%
    filter(treated == TRUE) %>%
    filter(retrofit_end_year == ty | consyear > retrofit_end_year) %>%
    mutate(treated = (retrofit_end_year == ty)) %>%
    group_by(treated, consyear) %>%
    summarise(energy = mean(energy, na.rm=T))

did_coef <- feols(log(energy) ~ treated * (consyear > ty) | treated + consyear , data=newdat)  

newdat %>%
  ggplot(aes(x=consyear, y=energy, colour=treated)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = ty, linetype="dashed") +
    theme_bw() +
    labs(x=NULL,
         y="Energy (GJ)",
         title=paste("Treated in ", ty, " vs earlier treated")) +
  annotate(geom="label", x=ty+1, y=mean(newdat$energy), label=paste("DiD: ", round(did_coef$coeftable$Estimate*100, 0), "%"))
}

le(2009)
ggsave("../output_figures_tables/le2009.png", width=6, height=4)
le(2010)
ggsave("../output_figures_tables/le2010.png", width=6, height=4)
le(2011)
ggsave("../output_figures_tables/le2011.png", width=6, height=4)
le(2012)
ggsave("../output_figures_tables/le2012.png", width=6, height=4)


# Estimate Callaway and Sant'anna model
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
  dplyr::select(id, consyear, retrofit_start_year, log_energy)


m_cs_all <- att_gt(
  yname = "log_energy",
  tname = "consyear",
  idname = "id",
  gname = "retrofit_start_year",
  control_group = "notyettreated",
  xformla = ~1,
  data = rd_stag_cs,
  allow_unbalanced_panel = TRUE,
  base_period = "universal"
)

aggte(m_cs_all, type="dynamic", na.rm=T)

m_cs_partic <- att_gt(yname = "log_energy",
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

aggte(m_cs_partic, type="dynamic", na.rm=T)

# Alternative sun and abraham package
library(staggered)
m_sa_all <- staggered_sa(df = rd_stag %>%
               mutate(log_energy = log(energy)) %>%
               mutate(retrofit_end_year = 
                        if_else(is.na(retrofit_end_year), 3000, retrofit_end_year)) %>%
               filter(!is.na(log_energy),
                      log_energy != -Inf),
             i = "id",
             t = "consyear",
             g = "retrofit_end_year",
             y = "log_energy",
             estimand = "cohort")

# Event study; I get errors when specifying event times
m_sa_all <- staggered_sa(df = rd_stag %>%
                           mutate(log_energy = log(energy)) %>%
                           mutate(retrofit_end_year = 
                                    if_else(is.na(retrofit_end_year), 0, retrofit_end_year)) %>%
                           filter(!is.na(log_energy),
                                  log_energy != -Inf),
                         i = "id",
                         t = "consyear",
                         g = "retrofit_end_year",
                         y = "log_energy",
                         estimand = "eventstudy",
                         #eventTime = -4:10
                         )
