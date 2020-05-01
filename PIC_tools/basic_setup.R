# This is based off of some script from 
# /qfs/projects/forteproject/fortebaseline/analysis/scripts/create-pic-inputs.R

# Set up the environment on pic ----------------------------------------------
library(fortebaseline)
library(dplyr)
library(tidyr)
library(readr)
library(udunits2) # Inorder to install on pic on R/3.4.3 had to contact pic (support because it required specific pacakge dependencies.)

# Because of some werid depency issues on pic it is difficult to build and 
# install the pecan porject but certain functions can become avaiable by sourcing them! 
# TODO is is possible to install the pecan package with udunits2 installed now? 
source('/people/dorh012/forte-workflow/pecan/models/ed/R/read_ed2in.R')
source('/people/dorh012/forte-workflow/pecan/models/ed/R/write_ed2in.R')

# Define Function -----------------------------------------------------------
# TODO modify this to suport how default run
# Use this function so set up the framework to run ED a but of times 
# 
# Args
#   case: a object that has information about the ed case to run
#   default: A TRUE or FALSE indicator that will be used to determine if the run is the default of not. 
setup_run <- function(case, default = FALSE){
  
  # Check function arguments. 
  assertthat::assert_that(is.null(case) | is.list(case))
  assertthat::assert_that(!default | default)
  assertthat::assert_that(is.null(case) & default || !is.null(case) & !default, msg = 'case can be NULL but then default must be set to false')
  assertthat::assert_that(dir.exists(inputdir))
  
  # Create the DIR to store the data in. 
  if(default){
    casename <- sprintf("%03d%s", case$param_id, case$case)
    outdir   <- file.path(local_basedir, "forte-ed-runs", "cases", 'default')
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    config_path <- ""
    
  } else {
    
    # Check the case set up if and only if the case actually exists. 
    assertthat::assert_that(all(c('param_id', 'case') %in% names(case)))
    
    casename <- sprintf("%03d%s", case$param_id, case$case)
    outdir   <- file.path(local_basedir, "forte-ed-runs", "cases", casename)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    
    # Only if there is paramter information being passed in with the cases. 
    config_xml <- as_pft_list(case$data) %>% fortebaseline::write_ed2_xml()
    config_path <- file.path(outdir, "config.xml")
    XML::saveXML(config_xml, config_path)
    
  }
  
  # Read in the tempale ED2IN file. 
  ed2in_template_file <- file.path("/qfs", "people", "dorh012", "forte-workflow", "testing-ensemble", "ED2IN")
  assertthat::assert_that(file.exists(ed2in_template_file), msg = paste0('template ED2IN file is missing from ', ed2in_template_file))
  ed2in_template      <- read_ed2in(ed2in_template_file) # the read_ed2in if a function defined in read_ed2in.R
  
  remote_input_dir <- file.path(remote_basedir, "ed")
  remote_output_dir <- file.path(remote_basedir, "forte-ed-runs")
  assertthat::assert_that(file.exists(remote_input_dir), msg = 'could not find remote_input_dir')
  
  
  # Import the soil data. 
  soil_data <- fortebaseline::umbs_soil()
  
  ed2in_tags <- list(
    # Start and end date  
    # TODO should these be hard coded in? 
    IYEARA = 1994, IMONTHA = 6, IDATEA = 1,
    IYEARZ = 1998, IMONTHZ = 1, IDATEZ = 1,
    
    # Site information 
    POI_LAT = 45.5625, POI_LON = -84.6975,
    
    # Define where to write output files to. 
    FFILOUT = file.path(remote_output_dir, "cases", casename, "analysis"),
    SFILOUT = file.path(remote_output_dir, "cases", casename, "history"),
    
    
    # Define the paths to the inputs. 
    IEDCNFGF = config_path,                                                   # the path to the configuration file
    VEG_DATABASE = file.path(remote_input_dir, "EDI", "oge2OLD", "OGE2_"),    # this is the path to the vegetation data base, this data is not relevant  see https://github.com/FoRTExperiment/ed4forte/issues/4
    SOIL_DATABASE = file.path(remote_input_dir, "EDI", "faoOLD", "FAO_"),     # Path to a  soil data base that contains information about soil texture ect.
    ISOILFLG = 2,  # A that determines if the soil data base should be used or not. 
    
    LU_DATABASE = file.path(remote_input_dir, "EDI", "ed_inputs", "glu"),
    THSUMS_DATABASE = file.path(remote_input_dir, "EDI", "ed_inputs/"),
    ED_MET_DRIVER_DB = file.path(remote_input_dir, "met", "CUSTOM_ED2_site_1-33", "ED_MET_DRIVER_HEADER"),
    
    # UMBS soil characteristics (from Gough et al. 2010 FEM)
    NSLCON = 1, # Sand
    SLXCLAY = 0.01,
    SLXSAND = 0.92,
    # Soil moisture data from Ameriflux
    # See analysis/scripts/soil-moisture.R of the fortebaseline project, https://github.com/ashiklom/fortebaseline
    # This soil information is pulled from the fortebaseline::umbs_soil(). 
    NZG = nrow(soil_data),          # SLZ for each grid cell
    SLZ = soil_data[["depth"]],     # Depth (m) of bottom of soil model grid levels
    SLMSTR = soil_data[["slmstr"]], # Initial soil moisture (fraction of saturation)
    
    # Misc ED parmaters 
    CROWN_MOD = 0, # Specifies how tree crowns are represent in the canopy radiation model the default is 0
    ECONOMICS_SCHEME = 0,  # Temporary variable for testing the relationship amongst traits in the tropics, but required by our version of ED. Default is set to 0.
    IHRZRAD = 0,     # Specifies how horizontal canopy radiation is solved. Default is set to 0. 
    
    # Intergration solver set up
    INTEGRATION_SCHEME = 1, # Runge–Kutta integration solver
    RK4_TOLERANCE = 1e-7,   # The tolerence for the integration solver, only applicable for Runge–Kutta
    
    # Output file set up (0 means no 3 means HDF5 output)
    IMOUTPUT = 3,      # Return monthly means, 1 HDF5 file per month
    IOOUTPUT = 0,      # Observation time output, turned off 
    MONTH_YRSTEP = 7,  # Month in which the yearly time step (patch dynamics) should occur, the default is set to 7
    IGOUTPUT = 0       # If IHRZRAD is not 0 then write patch table and gap relization files. 
    
  )
  
  # Modify the ED2IN file with the updated information
  ed2in <- modifyList(ed2in_template, ed2in_tags)
  
  # Write the ed2 in file out 
  write_ed2in(ed2in, file.path(outdir, "ED2IN"), barebones = TRUE) 
  invisible(outdir)
  
  
}

# ED Instance Set Up -----------------------------------------------------------
# Define the the input paths. 
remote_basedir <- file.path("/qfs", "people", "dorh012", "forte-workflow", "testing-ensemble")
inputdir      <- remote_basedir
local_basedir <- remote_basedir

# Load parameter inputs, these are from Alexey's GCB paper. 
param_draws <- read_csv("input-parameters.csv", col_types = cols(name = "c", .default = "d"))

# Format the parameters to be used in the function.
param_draws <- param_draws[1:3, ] 
param_draws <- param_draws[ , names(param_draws) %in% c('param_id', 'name', 'root_turnover_rate')]
param_draws[,3] <- 0.19735 
nparams <- nrow(param_draws)

param_nest <- param_draws %>% nest(-param_id)


# Create a data frame of the different strucutres, here because of the 
# way GBC paper there were muliple types of model stcutures that were being 
# examined, here set them all to FALSE. 
structures <- tibble(
  crown_model = c(FALSE),
  multiple_scatter = c(FALSE),
  trait_plasticity = c( FALSE)
) %>%
  expand(., !!!(syms(colnames(.)))) %>%
  mutate(case = paste0(
    if_else(crown_model, "F", "C"),
    if_else(multiple_scatter, "M", "T"),
    if_else(trait_plasticity, "P", "S")
  )) %>%
  select(case, everything())

# Select a single case set up
cases <- crossing(structures, param_nest) %>% arrange(param_id)
cases <- purrr::transpose(cases)
case <- cases[[1]]

# Generate the ED Input Structure  -----------------------------------------------------------
setup_run(case = case, default = FALSE)

# RUN ED
# !/bin/bash
# cd /qfs/people/dorh012/forte-workflow/testing-ensemble/forte-ed-runs/cases/001CTS/
# /people/dorh012/ed-source-code/ed_2.2-opt ./ED2IN
