# This file sources all R scripts

# Start by loading required libraries
library(tidyverse)
library(fixest)
library(janitor)
library(haven)
library(lubridate)
library(patchwork)
library(did)
library(broom)
library(corrplot)
library(vtable)

source(here::here("scripts", "01_read_processed_data.R"))
source(here::here("scripts", "02_graphical_analysis.R"))
source(here::here("scripts", "03_regression_twfe_overall_savings.R"))
source(here::here("scripts", "04_overall_event_study.R"))
source(here::here("scripts", "05_measure_by_measure.R"))
source(here::here("scripts", "06_realization_rate.R"))