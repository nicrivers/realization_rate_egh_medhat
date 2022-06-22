## Realization rate

Project for estimating the realized energy savings from Canadian energy efficiency programs, using data from Medicine Hat.

To reproduce all figures and tables, execute the batch file (Windows OS):
"run_all.bat"

OR 

within R, execute the script:
"scripts/00_source_all.R"

Execution of the scripts requires the file:
"processed_data/Final_merge_step_2_control_group_anonymous.dta"

For the matching analysis, the following file is also required:
"raw_data/tax data - ksp.csv"

Execution of this package requires:
- R, version 4 or above
- a tex distribution
- R packages, as listed in "scripts/00_source_all.R"