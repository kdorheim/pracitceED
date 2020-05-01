## ED2.2_NonHarvest 
## Looking at events.f90 it looks like there are mulitple types of distrubences that can occur. 
## The purpose of this script is to look at the different types of disturbences we can use to 
## perturb ED. 

## Ths purpose of this script is to explore the harvest events. How can we perscibe disturbence events?
library('ed4forte')
library(dplyr)
library(tidyr)
library(ggplot2)

exe_path <- "/Users/dorh012/Documents/2020/ed2/ED/build/ed_2.2-opt" 
file.exists(exe_path)
options(ed4forte.ed2_exe = exe_path)

ed_input_dir <- "ed-input-data" 
assertthat::assert_that(dir.exists("ed-input-data"))

output_dir <- file.path(here::here(), 'Disturbence Events', 'ED2.2 NonHarvest')
dir.create(output_dir)

# A1. Baseline Run ------------------------------------------------------------------------------------
results_dir <- file.path(output_dir, 'baseline')
dir.create(results_dir)
p <- run_ed2(
  results_dir,
  "2000-01-01",
  "2004-01-01",
  ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
  overwrite = TRUE
)
p$wait()
results_baseline  <- read_monthly_dir(results_dir)

# A2. mortatlity1  -------------------------------------------------------------------------------
event_xml <- here::here('Disturbence Events', 'events', 'thinning1.xml')
assertthat::assert_that(file.exists(event_xml))

results_dir <- file.path(output_dir, "thinning-ed-outputs")
dir.create(results_dir)

p <- run_ed2( results_dir,
              "2000-01-01",
              "2004-01-01",
              ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
              configxml = data.frame(),
              EVENT_FILE = event_xml,
              overwrite = TRUE)
p$wait()
results_m1 <- read_monthly_dir(results_dir)

# A3. mortatlity2  -------------------------------------------------------------------------------
event_xml <- here::here('Disturbence Events','events', 'mortality2.xml')
assertthat::assert_that(file.exists(event_xml))

results_dir <- file.path(output_dir, "mortality2-ed-outputs")
dir.create(results_dir)

p <- run_ed2( results_dir,
              "2000-01-01",
              "2004-01-01",
              ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
              configxml = data.frame(),
              EVENT_FILE = event_xml,
              overwrite = TRUE)
p$wait()
results_m1 <- read_monthly_dir(results_dir)

# A4. mortatlity3  -------------------------------------------------------------------------------
event_xml <- here::here('Disturbence Events', 'events', 'mortality3.xml')
assertthat::assert_that(file.exists(event_xml))

results_dir <- file.path(output_dir, "mortality3-ed-outputs")
dir.create(results_dir)

p <- run_ed2( results_dir,
              "2000-01-01",
              "2004-01-01",
              ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
              configxml = data.frame(),
              EVENT_FILE = event_xml,
              overwrite = TRUE)
p$wait()
results_m1 <- read_monthly_dir(results_dir)


