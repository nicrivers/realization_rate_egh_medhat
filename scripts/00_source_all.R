# This file sources all R scripts

# Start by loading required libraries
library(tidyverse)
library(fixest)
library(janitor)
library(haven)
library(lubridate)
library(patchwork)
library(did)

source(here::here("scripts", "01_read_processed_data.R"))