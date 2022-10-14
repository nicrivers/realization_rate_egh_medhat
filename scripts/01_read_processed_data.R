# This file reads processed data from Kareman (in Stata format)

dat <- read_stata(
    here::here(
        "processed_data", "final_merge_step_2_control_group_anonymous.dta"
    ),
    col_select = c(
        "PreretrofitENTRYDATE",
        "PostretrofitENTRYDATE",
        "ID",
        "consyear",
        "consmonth",
        "E11",
        "G11A",
        contains("Done"),
        contains("Electicalconsum"),
        contains("Electricalconsum"),
        contains("gasconsum"),
        "PreRetrofitFURNACETYPE"
        )) %>%
  clean_names() %>%
  dplyr::rename(elec=e11, gas=g11a) %>%
  mutate(cons_date=as.Date(paste(consyear, consmonth, "01",sep="-")))

# Define total energy consumption by adding gas an electricity
dat <- dat %>%
  # Gas is in thousands of cubic feet.
  # *** SHOULD CHECK THIS ***
  # Convert to GJ by multiplying by 1.0551
  # See: https://www.nrcan.gc.ca/energy/energy-sources-distribution/natural-gas/natural-gas-primer/5641#conversion
  # Electricity is in kWh
  # Convert to GJ by dividing by 277.778
  mutate(
    energy = gas*1.0551 + elec/277.778
  ) %>%
  # For total energy consumption, need to convert all units to Gj
  # 1 kWh = 0.0036 GJ
  # 1 m3 = 0.0373 GJ: https://www.nrcan.gc.ca/energy/energy-sources-distribution/natural-gas/natural-gas-primer/5641
  mutate(
    postretrofit_energy = postretrofit_electricalconsump * 0.0036 + postretrofit_naturalgasconsum * 0.0373,
    preretrofit_energy = preretrofit_electicalconsumpti * 0.0036 + preretrofit_naturalgasconsum * 0.0373
  ) %>%
  # express all in GJ per year
  mutate(
    actual_gas_gj_per_yr = gas * 1.0551 * 12,
    actual_elec_gj_per_yr = elec / 277.778 * 12,
    actual_energy_gj_per_yr = energy * 12,
    predicted_preretrofit_gas_gj_per_yr = preretrofit_naturalgasconsum * 0.0373,
    predicted_preretrofit_elec_gj_per_yr = preretrofit_electicalconsumpti * 0.0036,
    predicted_preretrofit_energy_gj_per_yr = preretrofit_energy,
    predicted_postretrofit_gas_gj_per_yr = postretrofit_naturalgasconsum * 0.0373,
    predicted_postretrofit_elec_gj_per_yr = postretrofit_electricalconsump * 0.0036,
    predicted_postretrofit_energy_gj_per_yr = postretrofit_energy,
  )

# Only keep households with both gas and electricity consumption
with_energy <- dat %>%
  group_by(id) %>%
  summarise(across(c(gas, elec, energy), mean, na.rm=T)) %>%
  filter(!is.na(gas) & !is.na(elec)) %>%
  #filter(!is.na(gas) | !is.na(elec)) %>% # Use this to keep houses with either gas or electricity
  dplyr::select(id)

dat <- inner_join(dat, with_energy)



# Load the tax data
taxdat <- read_csv("../raw_data/tax - ksp.csv") %>%
  rename(id = umLocationID)

# Merge with data
dat <- left_join(dat, taxdat)

# Create regression data
# Define treated and post variables
rd <- dat %>%
  mutate(treated = !is.na(postretrofit_entrydate),
         post = (cons_date > postretrofit_entrydate),
         treated_post = if_else(treated & post, TRUE, FALSE),
         year = year(cons_date)) %>%
  # Drop all energy consumption observations between the pre- and post-retrofit
  filter(!treated |
           (treated & (cons_date > postretrofit_entrydate | cons_date < preretrofit_entrydate)))


