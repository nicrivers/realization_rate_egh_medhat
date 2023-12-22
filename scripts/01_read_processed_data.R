# This file reads processed data from Kareman (in Stata format)

dat <- read_stata(
    here::here(
        "processed_data", "final_merge_step_2_July_control_anonymous.dta"
    ),
    col_select = c(
        "PreretrofitENTRYDATE",
        "PostretrofitENTRYDATE",
        "ID",
        "consyear",
        "consmonth",
        "E11",
        "G11A",
        "PreRetrofitHEATAFUE",
        contains("Done"),
        contains("Recommend"),
        contains("Electicalconsum"),
        contains("Electricalconsum"),
        contains("gasconsum"),
        "PreRetrofitFURNACETYPE" 
        # incentive variables
        ,contains("paid")
        )) %>%
  clean_names() %>%
  dplyr::rename(elec=e11, gas=g11a) %>%
  mutate(cons_date=as.Date(paste(consyear, consmonth, "01",sep="-")))

# Define total energy consumption by adding gas an electricity
dat <- dat %>%
  # Gas is in thousands of cubic feet.
  # *** CHECK TO BE SURE ***
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

# Original data
nh_orig = length(unique(dat$id))
paste("The original data contains ", nh_orig, " households, including", length(unique(dat$id[!is.na(dat$postretrofit_entrydate)])), " participants and ", length(unique(dat$id[is.na(dat$postretrofit_entrydate)])), " non-participants.")

# Only keep households with both gas and electricity consumption
with_energy <- dat %>%
  group_by(id) %>%
  summarise(gas = mean(gas, na.rm=T),
            elec = mean(elec, na.rm=T)) %>%
  filter(!is.na(gas) & !is.na(elec)) %>%
  dplyr::select(id)

dat <- inner_join(dat, with_energy)

# After dropping houses missing energy data
nh_en = length(unique(dat$id))
paste("We drop ", nh_orig-nh_en, " households that report either no elec or gas data. We now have ", length(unique(dat$id[!is.na(dat$postretrofit_entrydate)])), " participants and ", length(unique(dat$id[is.na(dat$postretrofit_entrydate)])), " non-participants.")


# Load the tax data
taxdat <- read_csv("../raw_data/tax - ksp.csv") %>%
  rename(id = umLocationID) 
# There are some ids with multip buildings. Mostly commerical. Delete
taxdat <- taxdat %>%
  group_by(id) %>%
  summarise(n=n()) %>%
  filter(n==1) %>%
  dplyr::select(id) %>%
  inner_join(taxdat) %>%
  filter(! Building_Type %in% c("NO MARKET BUILDING CLASS",
                                "OFFICE BUILDING",
                                "RETAIL STORE",
                                "WAREHOUSE",
                                "GARAGE",
                                "MEDICAL OFFICE",
                                "BARN",
                                "STORE",
                                "ELEVATOR",
                                "APARTMENT"
                                ))


# Merge with data: we only keep observations for which we have tax data
dat <- inner_join(dat, taxdat)

# After dropping houses missing tax data
nh_tx = length(unique(dat$id))
paste("We drop ", nh_en-nh_tx, " households for which we cannot match with tax data")
paste("We are left with a data set containing ", nh_tx, " households including ", length(unique(dat$id[!is.na(dat$postretrofit_entrydate)])), " participants and ", length(unique(dat$id[is.na(dat$postretrofit_entrydate)])), " non-participants.")


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


# Set up annual data
rd_stag <- rd %>%
  mutate(retrofit_start_year = year(preretrofit_entrydate),
         retrofit_end_year = year(postretrofit_entrydate),
         years_to_treatment = case_when(
           consyear < retrofit_start_year ~ consyear - retrofit_start_year,
           consyear > retrofit_end_year ~ consyear - retrofit_end_year,
           is.na(retrofit_end_year) ~ -1000,
           consyear >= retrofit_start_year & consyear <= retrofit_end_year ~ -2000
         )) %>%
  group_by(id,consyear,treated,years_to_treatment,retrofit_start_year, retrofit_end_year) %>%
  summarise(elec=mean(elec, na.rm=T),
            gas=mean(gas, na.rm=T),
            energy=mean(energy, na.rm=T)) %>%
  mutate(treated_post = (consyear > retrofit_end_year)) %>%
  # Prior command generates NA for households that don't have retrofit end year. Replace with FALSE
  replace_na(list(treated_post = FALSE))
