## Following ED 2.2 R interface Tutorial from 
## https://github.com/FoRTExperiment/ed4forte/blob/master/documentation/02-ed2-r-interface.md
# devtools::install_github("FoRTExperiment/ed4forte")

# Locate the ed executable you must use the executable! 
exe_path <- "/Users/dorh012/Documents/2020/ed2/ED/build/ed_2.2-opt" 
file.exists(exe_path)
options(ed4forte.ed2_exe = exe_path)

library(ed4forte)
library(dplyr)

outdir <- "test-ed-outputs"
ed_input_dir <- "ed-input-data"

p <- run_ed2(outdir = outdir, start_dt = "2000-01-01", end_dt = "2001-01-01", 
             ED_MET_DRIVER_DB = file.path(ed_input_dir, "NARR-ED2", "ED_MET_DRIVER_HEADER"))

# Take a look at the output logs. 
readLines(p$get_output_file())

# Check the status of the run 
p$get_status()
p$is_alive()

# List all of the files returned as output
list.files(outdir)

# Now let's play around with the output. 
outfiles <- list.files(outdir, "analysis-E-", full.names = TRUE)

# Let's import the first file and extract a single file 
nc <- ncdf4::nc_open(outfiles[1])
ncdf4::ncvar_get(nc, "AGB_CO")

# What is the cariable AGB_CO anyways? 
ed4forte:::ed2_variables %>% 
  filter(variable == "AGB_CO") %>%
  pull(description)

# Ah it is above ground biomass 

# Let's checkout the mean GPP 
ncdf4::ncvar_get(nc, "MMEAN_GPP_PY")


# Because the ED data writes out a single hdf5 file at a time we will use
# a support function to read in mulitple files. 
results <- read_monthly_dir(outdir)
help(read_monthly_dir)


# Let's checkout the object returned by the function 
str(results)  # some sort of listed 
dim(results)  # a list with 6 elements 

# A bit about the results strucutre 
# It is a list of nested tibbles this is a legacy strucutre from one of AS older projects 
# It returns 4 data frames 
# Because different variables have different “grids” associated with them.
# So the scalar values have one value per timestep; the cohort variables have ncohorts 
# rows per timestep (along with an additional index that identifies the cohort); 
# the soil variables have n_soil_depth rows per timestep; etc.
# Where a cohort is a group of trees of the same age and PFT, within a patch
## IMPORTANT NOTE 
## The NPLANT cohort variable gives you the number of trees of that cohort in a stand 
## (technically, it’s the density, but the site area is fixed, so it’s just a multiple 
## of the number of trees). The weird thing about ED is that this a continuous quantity,
## even though in reality, tree counts are discrete. In other words, it’s perfectly 
## normal for ED2 to have, say, 0.7 or 1.4 “trees” within a cohort.
## A lot of the ED2 output variables are given in units of per plant, (e.g. kgC/pl), 
## so NPLANT is a quantity you’ll need to use a lot for normalizing those to area.

results$basename
results$outdir

results$df_scalar %>% length()
intersect(names(results$df_scalar[[1]]), names(results$df_cohort[[1]]))

ed4forte::ed2_variable_info() %>% 
  filter(variable == 'XATM') %>%  
  pull(description)
# Atm = atmosphere but this information is really only useful in regional analyses 

ed4forte::ed2_variable_info() %>% 
  filter(grepl(tolower('NPLANT'), tolower(variable))) %>% 
  pull(description)

# Variable naming pattern 
# _PY suffix generally means the PFT x DBH x polygon averaging
# _CO suffix means by cohort.


# In order to look at the data you have to index in one, 
# which is where the tidyr::unnest function comes in 
# pretty hanny 
results$df_soil[[1]] %>% dim()
results[['df_soil']] %>% dim()

# Play around with plotting the results. 
scalar_results <- tidyr::unnest(results, df_scalar)
plot(MMEAN_GPP_PY ~ datetime, data = scalar_results, type = "o",
     main = "Total plot GPP")

pft_results <- tidyr::unnest(results, df_pft)
pft_results

plot(MMEAN_LAI_PY ~ datetime, data = pft_results, col = pft, pch = 19,
     main = "LAI by PFT")


ed4forte::ed2_variable_info() %>%  
  filter(variable == 'SLZ')


results$'df_soil'[[1]] %>% names()

results$'df_soil'[[1]]$SLZ %>%  unique()


results$df_cohort[[1]] %>%  names()
ed2_variable_info(variables = 'BA_CO')
results$df_cohort[[1]]$AGB_CO %>% unique()
