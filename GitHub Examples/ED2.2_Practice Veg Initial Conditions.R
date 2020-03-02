## https://github.com/FoRTExperiment/ed4forte/blob/master/documentation/03-initial-conditions.md
## Following allong with the example that AS shared. 
## The purpose of this script it to set up an ED run with inital vegiation condtions. 
## 
## Questions 
##    okay why does this matter? Is this comparable to ensemble members for cliamte models? 
##    why do are have mulitple sites and what does that mean? 
## THIS EXAMPLE FAILS 

# 0. Set Up --------------------------------------
# Load the required packages. 
library(ed4forte)
library(fortedata)
library(ggplot2)

# Define the path to the ed2 exectuable. 
exe_path <- "/Users/dorh012/Documents/2020/ed2/ED/build/ed_2.2-opt" 
file.exists(exe_path)
options(ed4forte.ed2_exe = exe_path)

run_dir <- here::here('GitHub Examples', 'ED2.2_Practice Veg Inital Conditions'); 
dir.create(run_dir, showWarnings = FALSE)
# 1. Define Imnital Conditions ------------------- 
# TBH I am pretty confused as to why the here calls link to the 
# ed4forte project but alas 
narr_ed <- here::here("unsynced-data", "ed-input-data", "NARR-ED2",
                      "ED_MET_DRIVER_HEADER")
outdir <- file.path("forte-inits") # here::here("unsynced-data", "ed2-outputs", "forte-inits")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Createa vegetation intial condition from the forte data invetnroy files.  
prefix  <- file.path(outdir, "fortedata")
css_pss <- fortedata2ed(output_prefix = prefix)
# Format the file name with coordinates (lat/lon) to a specfic location. 
coords_prefix(prefix, "css")


p <- run_ed2(
  # NOTE: 4.5 year simulation -- could take a few minutes...
  outdir, "2000-06-01", "2005-01-01",
  ED_MET_DRIVER_DB = narr_ed,
  # Control how the plant community and soil carbon pools are set up. 
  # When set to 6 sets up mulitple sites and  the PFT types are preserved. 
  IED_INIT_MODE = 6, 
  # Contorl what PTFs are going to be modeled in the run. 
  # Only include North pine (6), late conifer (8), and early (9), mid (10), and
  # late (11) temperate hardwoods.
  INCLUDE_THESE_PFT = c(6, 8:11),
  SFILIN = prefix
)
get_status(p)
p$wait()

# Because there are mulitple sites the read montly data is not going to be happy, 
# so we have to extract by hand, althought I wonder if there would be a way to 
# modify this into a funciton. 
getvar <- function(f, v) {
  nc <- ncdf4::nc_open(f)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncvar_get(nc, v)
}
files <- list.files(outdir, "analysis-E", full.names = TRUE)

dat <- tibble::tibble(
  # Pull the dates out from the file names and format it as a data object 
  dates = stringr::str_extract(files, "[[:digit:]]{4}-[[:digit:]]{2}") %>%
    paste0("-01") %>%
    as.Date(),
  # extract the hight of the coohorts 
  hite = lapply(files, getvar, "HITE"),
  # extract the dbh retults 
  dbh = lapply(files, getvar, "DBH"),
  # extract the class of the pfts 
  pft = lapply(files, getvar, "PFT")
)

# Because we have mulitple sites and the cohotrts are not consistent over the 
# months there is a number of processing htat has to take place before we can plot it. 
dat %>%
  tidyr::unnest(c(hite, dbh, pft)) %>%
  # Structure doesn't change that often. Let's look at July of every year.
  # Only pulling out restults for JULY wow that is neat 
  dplyr::filter(lubridate::month(dates) == 7) %>%
  dplyr::group_by(dates) %>%
  # Remeber that each row is a cohort that does not have a unique id. 
  # and they are different between each MONTH! 
  dplyr::mutate(icohort = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  # Now lets make a plot!
  ggplot() +
  aes(x = icohort, xend = icohort, yend = 0, y = hite, size = dbh, color = factor(pft)) +
  geom_segment(size = 1) +
  geom_point() +
  facet_wrap(vars(dates)) +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_blank()) +
  ggtitle("Height and DBH, by cohort and PFT, for five ED2 simulation years.")
