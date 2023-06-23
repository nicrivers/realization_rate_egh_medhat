# Table of summary statistics

st(data = rd %>%
     group_by(id) %>%
     # For non-participants, replace NA with 0
     mutate(across(contains("done"), ~replace_na(.,0))) %>%
     summarise(across(c(contains("done"),contains("gj_per_yr"), contains("value"), contains("size"), contains("yearbuild")), mean, na.rm=T)) %>%
     dplyr::select(-id, - ashp_upgrade_done, -gshp_upgrade_done, -oil_furnace_upgrade_done, -dhw_upgrade_done, -exp_floor_ugr_done, -type1ugr_done) %>%
     rename_with(., ~gsub("\\_ugr_done|\\_upgrade_done","", .x)) %>%
     mutate(non_participant = is.na(predicted_postretrofit_gas_gj_per_yr)) %>%
     mutate("Energy consumption data"=as.numeric(NA),
            "Property assessment data"=as.numeric(NA),
            "Program participation data"=as.numeric(NA)) %>%
     # Order variables correctly
     dplyr::select( "non_participant",
                    "Energy consumption data",
                    "Actual gas consumption (GJ/year)"="actual_gas_gj_per_yr", "Actual electricity consumption (GJ/year)"="actual_elec_gj_per_yr", "Actual energy consumption (GJ/year)"="actual_energy_gj_per_yr",
                    
                    "Property assessment data",
                    "Total assessed value ($)"="TotalAssesmentValue", "Lot size (square metres)"="LotSize", "Building size (square metres)"="BuildingSize", "Year built"="EffectiveYearBuild", 
                    
                    "Program participation data",
                    "Air sealing"="air_sealing", "Attic insulation"="ceiling_insulation", "Wall insulation"="walls_insulation", "Basement insulation"="bsmt_insulation", "Foundation header insulation"="fnd_header", "Window or door upgrade"="windowsand_doors", "Central A/C upgrade"="central_ac", "Natural gas furnace upgrade"="natural_gas_furnace", 
                    "Predicted pre-retrofit gas consumption (GJ/year)"="predicted_preretrofit_gas_gj_per_yr", "Predicted pre-retrofit electricity consumption (GJ/year)"="predicted_preretrofit_elec_gj_per_yr", "Predicted pre-retrofit energy consumption (GJ/year)"="predicted_preretrofit_energy_gj_per_yr", "Predicted post-retrofit gas consumption (GJ/year)"="predicted_postretrofit_gas_gj_per_yr", "Predicted post-retrofit electricity consumption (GJ/year)"="predicted_postretrofit_elec_gj_per_yr", "Predicted post-retrofit energy consumption (GJ/year)"="predicted_postretrofit_energy_gj_per_yr"),
   group = "non_participant",
   file = "../output_figures_tables/both_sumtable.tex",out="latex",
   title = "Summary statistics\\label{tab:sumstat}")
