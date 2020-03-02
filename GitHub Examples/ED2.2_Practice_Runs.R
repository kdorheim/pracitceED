## ED2.2_Pracitce_Runs 
## The oobjective of this sciprt is to run ED  and change the model start and end time. 
## Also to change some paramters -- do I understand how to change the parameters and 
## the effect this may have on the output? 
##
## Ideally I would like to do this via the ED 2 R interface but aslo from the 
## command line and them compare the results. 
## 
## I think that there should probably be some better way to extract the results after 
## the monthly_output.rds file has been generated. Because ED does take a while to 
## run it is pretty annoying to have to do that 

# 0. Set Up -------------------------------------
library(ed4forte)
#devtools::load_all('~/Documents/2020/ed4forte/')
library(dplyr)
library(tidyr)
library(ggplot2)

exe_path <- "/Users/dorh012/Documents/2020/ed2/ED/build/ed_2.2-opt" 
file.exists(exe_path)
options(ed4forte.ed2_exe = exe_path)

run_dir <- here::here('GitHub Examples', 'ED2.2_Practice_Runs'); 
dir.create(run_dir, showWarnings = FALSE)


# tidy up the ed restuls 
# 
# Because the ed results are returned as a nested list a bit of 
# formatting must be done to make sure that the output is returned 
# in a format that is easy to plot. 
# 
# Args
#   rlst: the result returned by the read_monthly_dir
#   name: a string of one of the df names that needs to be unlisted. 
# Returns: a single data frame of information
tidy_ed_results <- function(rlst, name){
  
 dplyr::bind_rows(rlst[[name]][[1]]) %>% 
    mutate(name = rlst$basename)
  
}



# A Standard Runs ----- 
# A1. Baseline Run -------------------------------------
## This is our baselien run from 2000 to 2001 with the 
## default configuration. Other results 
outdir       <- file.path(run_dir, "baseline-ed-outputs")
ed_input_dir <- "ed-input-data" 
assertthat::assert_that(dir.exists("ed-input-data"))

p <- run_ed2(
  outdir,
  "2000-01-01",
  "2001-01-01",
  ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
  overwrite = TRUE
)
p$wait()
outfiles <- list.files(outdir, "analysis-E-", full.names = TRUE)
results  <- read_monthly_dir(outdir)

baseline_scalar_rslt <- tidy_ed_results(results, "df_scalar")
baseline_cohort_rslt <- tidy_ed_results(results, "df_cohort")
baseline_soil_rslt   <- tidy_ed_results(results, "df_soil")
baseline_pft_rslt    <- tidy_ed_results(results, "df_pft") 

# A2. Shorter Time -------------------------------------
## This is our baselien run from Jan to October of 2000 with the 
## default configuration. Other results 
outdir       <-  file.path(run_dir, "short-ed-outputs")
ed_input_dir <- "ed-input-data" 
assertthat::assert_that(dir.exists("ed-input-data"))

p <- run_ed2(
  outdir,
  "2000-01-01",
  "2000-10-01",
  ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"), 
  overwrite = TRUE
)
p$wait()
outfiles <- list.files(outdir, "analysis-E-", full.names = TRUE)
results  <- read_monthly_dir(outdir)

short_scalar_rslt <- tidy_ed_results(results, "df_scalar")
short_cohort_rslt <- tidy_ed_results(results, "df_cohort")
short_soil_rslt   <- tidy_ed_results(results, "df_soil")
short_pft_rslt    <- tidy_ed_results(results, "df_pft") 


# A3. Longer Time ------------------------------------
## This is our baselien run from 2000 to 2002 with the 
## default configuration. Other results 
outdir       <-  file.path(run_dir,"long-ed-outputs")
ed_input_dir <- "ed-input-data" 
assertthat::assert_that(dir.exists("ed-input-data"))

p <- run_ed2(
  outdir,
  "2000-01-01",
  "2002-10-01",
  ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"), 
  overwrite = NULL
)
p$wait()
outfiles <- list.files(outdir, "analysis-E-", full.names = TRUE)
results  <- read_monthly_dir(outdir)

long_scalar_rslt <- tidy_ed_results(results, "df_scalar")
long_cohort_rslt <- tidy_ed_results(results, "df_cohort")
long_soil_rslt   <- tidy_ed_results(results, "df_soil")
long_pft_rslt    <- tidy_ed_results(results, "df_pft") 

# A4. Consolidate R Results ---------------------------
## Because eventually we are going to be comparing the results from the 
## R interface and commandline I want to consolidate the restults 
## into a single data frame to make it easier to plot. 
scalar <- bind_rows(baseline_scalar_rslt, short_scalar_rslt, long_scalar_rslt)
cohort <- bind_rows(baseline_cohort_rslt, short_cohort_rslt, long_cohort_rslt)
soil   <- bind_rows(baseline_soil_rslt, short_soil_rslt, long_soil_rslt)
pft    <- bind_rows(baseline_pft_rslt, short_pft_rslt, long_pft_rslt)

scalar$source <- 'R' 
cohort$source <- 'R' 
soil$source   <- 'R' 
pft$source    <- 'R' 

# Alrgithty so it turns out that plotting these is very complicated bcasue there
# are so many different spatial and temoporal scales to think about however 
# here we are going to plot a few different ones just to show that we can 
# maninpualte ED using the R interface. 
required_vars <- c('name', 'datetime')
scalar_vars <- c(required_vars, 'MMEAN_GPP_PY')
cohort_vars <- c(required_vars, 'MMEAN_GPP_CO', # this is in a per plant so we are going to have to mulitply it by N per cohort 
                 'NPLANT') # The number of plants per cohort) 
soil_vars <- c(required_vars, 'DMEAN_TRANSLOSS_PY', 'SLZ') 
pft_vars <- c(required_vars, 'AGB_PY' )

baseline_cohort_rslt %>% 
  select(cohort_vars, datetime) %>% 
  mutate(gpp = MMEAN_GPP_CO * NPLANT) %>% 
  group_by(datetime) %>%  
  summarise(value = sum(gpp)) %>%  
  ungroup ->
  agg_gpp
  
baseline_scalar_rslt %>% 
  select(datetime, scalar_vars) %>% 
  mutate(value = MMEAN_GPP_PY ) ->
  new_scalar_df
  
ggplot() + 
  geom_line(data = new_scalar_df, aes(datetime, value, color = 'scalar monthly', linetype = '1'), size = 2) +
  geom_line(data = agg_gpp, aes(datetime, value, color = 'aggregated cohort value', linetype = '2'), size = 2) + 
  labs(y = 'MMEAN GPP kgC/m2/yr', 
       title = 'Comparison off the cohort vs scalar GPP they should be identical')

# Now plot the difference between different runs. 
scalar %>% 
  select(scalar_vars) %>%  
  ggplot(aes(datetime, MMEAN_GPP_PY, color = name)) + 
  geom_point() + 
  facet_wrap('name')

cohort %>% 
  select(cohort_vars) %>% 
ggplot(aes(datetime, MMEAN_GPP_CO, color = name)) + 
  geom_point() + 
  facet_wrap('name')

# Well it looks like it 0 for everything hmm is that expected idk, 
# for now I think that we should assume that it is. 
soil %>% 
  select(soil_vars) %>% 
  ggplot(aes(datetime, DMEAN_TRANSLOSS_PY, color = name)) + 
  geom_line() + 
  facet_wrap('name')

pft %>% 
  select(pft_vars) %>% 
  ggplot(aes(datetime, AGB_PY, color = name)) + 
  geom_point() + 
  facet_wrap('name') + 
  labs()

## COOL so the plots of the manipulated baseline outputs looks good! 
## THAT's dope! 



# A5. Import the commandline results -------
# B Modify INI ----- 
outdir       <-  file.path(run_dir,"CROWN-ed-outputs")
ed_input_dir <- "ed-input-data" 
assertthat::assert_that(dir.exists("ed-input-data"))

p <- run_ed2(
  outdir,
  "2000-01-01",
  "2001-01-01",
  ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
  ICANRAD = 1,
  CROWN_MOD = 1,
  overwrite = TRUE
)
p$wait()
outfiles <- list.files(outdir, "analysis-E-", full.names = TRUE)
results  <- read_monthly_dir(outdir)

crown_scalar_rslt <- tidy_ed_results(results, "df_scalar")

ggplot() + 
  geom_line(data = crown_scalar_rslt, aes(datetime, MMEAN_GPP_PY)) + 
  geom_line(data = baseline_scalar_rslt, aes(datetime, MMEAN_GPP_PY, color = 'baseline'))

# What happens when we use the config xml to change ED 
p <- run_ed2(
  outdir,
  "2000-01-01",
  "2001-01-01",
  ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
  configxml = data.frame(
    num = c(9, 10), 
    SLA = c(35.3, 38.2),
    Vm0 = c(27.4, 24.3)
  ),
  overwrite = TRUE
)
p$wait()
results  <- read_monthly_dir(outdir)
config_scalar_rslt <- tidy_ed_results(results, "df_scalar")


ggplot() + 
  geom_line(data = crown_scalar_rslt, aes(datetime, MMEAN_GPP_PY)) + 
  geom_line(data = config_scalar_rslt, aes(datetime, MMEAN_GPP_PY, color = 'config')) +
  geom_line(data = baseline_scalar_rslt, aes(datetime, MMEAN_GPP_PY, color = 'baseline'))
