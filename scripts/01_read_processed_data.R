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
        contains("gasconsum"))) %>%
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
  )

# Create regression data
# Define treated and post variables
rd <- dat %>%
  mutate(treated = !is.na(postretrofit_entrydate),
         post = (cons_date > postretrofit_entrydate),
         treated_post = if_else(treated & post, TRUE, FALSE),
         year = year(cons_date))


