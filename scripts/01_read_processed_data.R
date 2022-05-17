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

