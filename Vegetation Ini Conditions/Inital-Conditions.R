## Following https://github.com/FoRTExperiment/ed4forte/blob/master/documentation/03-initial-conditions.md
## This script sets up ED to run from inital conditions instead of from a bear ground. 
## The results the inital cnoditions runs will be compared to inventory data. We are \
## intrested in weather or not onl LIA is low but how bout DBH and AGB? 
##
## Objectives: 
##   x Run ED using the inital vegitation condtiongs from UMBS. (uses the fortedata inventory data)
##   x Get comfy extracting data from the ed results but also from the fortedata pacakge. (right now it 
##      looks like ed is still underestimating output variables.)
## TODO will spinning ed up for a longer time help? yes we started initial vegitation conditions but what 
##      about other things? does it need to run for a longer time? 
# 0. Set Up -----------------------------------------------------------------------
# Import libs
library(ggplot2)
library(ed4forte)
library(magrittr)

# Save a copy of the lai data from fortedata before it was removed. 
devtools::load_all('~/Documents/2020/fortedata/')
lai_data <- fd_lai()

# Now install the up todate version of fortedata
devtools::install_github('FoRTExperiment/fortedata') 
library(fortedata)


# Define output and input directories. 
# Provide the path to the meterology data 
narr_ed <- here::here("ed-input-data", "NARR-ED2", "ED_MET_DRIVER_HEADER")
outdir <- here::here("unsynced-data", "ed2-outputs", "forte-inits")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# 1. Prep Inputs and Run ED -----------------------------------------------------------------------
# Only prep and run the inputs there are no hdf5 files in the output dir. 
hd5f_files <- any(grepl(pattern = 'analysis|.h5', x = list.files(outdir, pattern = '.h5')))

if(!hd5f_files){
  # There is somethign about the prefix which I don't really understand 
  prefix <- file.path(outdir, "fortedata")
  
  # The frotedata2ed is pulling data from the foretedata pacakge 
  css_pss <- fortedata2ed(output_prefix = prefix)
  
  
  # Run ED starting from inital conditions 
  p <- run_ed2(
    # NOTE: 4.5 year simulation -- could take a few minutes...
    outdir, "2000-06-01", "2005-01-01",
    ED_MET_DRIVER_DB = narr_ed,
    IED_INIT_MODE = 6,
    # Only include North pine (6), late conifer (8), and early (9), mid (10), and
    # late (11) temperate hardwoods.
    INCLUDE_THESE_PFT = c(6, 8:11),
    # This is reading in the path to the foretedat.blahblah which has the formatted inventory 
    # inforamtion. 
    SFILIN = prefix
  )
  get_status(p)
  p$wait()
}


# 2. Extract Inputs  -----------------------------------------------------------------------
# Extract output varaible from an ed hdf5 file. 
# Args
#     f: the file path of an ed hdf5 file
#     v: a string of a single ed variable name. 
getvar <- function(f, v) {
  nc <- ncdf4::nc_open(f)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncvar_get(nc, v)
}

# Find all of the ed hdf file outputs. 
files <- list.files(outdir, "analysis-E", full.names = TRUE)

# Extract the data from the hdf5 files, the resulting tibble will contain
# nested dataframes and will therefore need to be further processed. 
dat <- tibble::tibble(
  # Extract the date information from the  hdf file name. 
  dates = stringr::str_extract(files, "[[:digit:]]{4}-[[:digit:]]{2}") %>%
    paste0("-01") %>%
    as.Date(),
  # Extract ED data.  
  hite = lapply(files, getvar, "HITE"),
  dbh  = lapply(files, getvar, "DBH"),
  pft  = lapply(files, getvar, "PFT"), 
  lai  = lapply(files, getvar, "MMEAN_LAI_CO"),
  agb  = lapply(files, getvar, 'AGB_PY')
  )

# Process the nested tibble into a single data frame. 
dat %>% 
  tidyr::unnest(c(hite, dbh, pft, lai)) %>% 
  dplyr::group_by(dates) %>%
  # Add a cohort id number to each entry, note that the cohorts are not stable 
  # over the different time steps. 
  dplyr::mutate(icohort = dplyr::row_number()) %>%
  dplyr::ungroup() %>% 
  dplyr::select(-agb) ->
  ed_cohort_data

# Process the above ground biomass. 
# TODO need to check to see if this is something that we add together or take an avearge of. 
apply(dat, 1, function(d){
  sum(unlist(d[['agb']]), na.rm = TRUE)
}) %>%
  unlist -> 
  abg

ed_abg_data <- tibble::tibble(date = dat$dates, 
               value = abg, 
               variable = 'abg') 

# 3. Compare Ed and UMBS  -----------------------------------------------------------------------
# In this section we compare Ed and UMB data. Now part of the problem is going to be that I am not 
# sure which replica we are trying to plot and how the units of Ed and fortedata really compare with 
# one another. 

umbs <- fd_inventory()  %>% 
  dplyr::left_join(species_pfts(), by = 'Species') %>%  
  dplyr::mutate(source = 'umbs') 

ed_cohort_data %>% 
  dplyr::select(dbh, pft) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(source = 'ed') %>%  
  dplyr::bind_rows(umbs %>% 
                     dplyr::select(pft, dbh = DBH_cm, source)) ->
  dbh_data

ggplot(data = dbh_data) + 
  geom_point(aes(pft, dbh, fill = source, color = source), position = position_jitterdodge(), alpha = 0.5) + 
  theme_bw() + 
  labs(title = 'Comparison of ED and UMBS DBH by PFT', 
       y = 'DBH cm', 
       x = 'PFT') 











