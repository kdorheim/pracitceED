# run_ed2 documenation 


# Provide the path to the meterology data 
narr_ed <- here::here("ed-input-data", "NARR-ED2", "ED_MET_DRIVER_HEADER")
outdir <- here::here("unsynced-data", "ed2-outputs", "test")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# There is somethign about the prefix which I don't really understand 
prefix <- file.path(outdir, "fortedata")

# NOTE: 4.5 year simulation -- could take a few minutes...
outdir <- outdir
start_dt <- "2000-06-01"
end_dt <- "2001-01-01"
configxml = NULL
ED_MET_DRIVER_DB = narr_ed
ed2_exe = getOption("ed4forte.ed2_exe")
INCLUDE_THESE_PFT = c(6, 8:11)
overwrite <- TRUE
tz = "UTC"

# function (outdir, start_dt, end_dt, configxml = NULL, ed2_exe = getOption("ed4forte.ed2_exe"), 
#           wd = NULL, env = NULL, stdout = file.path(outdir, "stdout.log"), 
#           stderr = file.path(outdir, "stderr.log"), tz = "UTC", overwrite = NULL, 
#           ...) 
# {
# Check to see if the exectuable lives 
  stopifnot(!is.null(ed2_exe), file.exists(ed2_exe))
  
  # Create the output directory 
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # If there are h5 files then remove them if overwite is set up to be true
  h5_files <- list.files(outdir, ".h5")
  if (length(h5_files) > 0 && is.null(overwrite)) {
    overwrite <- askYesNo(msg = paste0(outdir, " contains already contains ed output do you want to delete these files before proceeding?"))
  }
  if (!is.null(overwrite) && isTRUE(overwrite)) {
    file.remove(h5_files)
  } else if (!is.null(overwrite) && is.na(overwrite)) {
    stop("run canceled")
  }
  # determine the time based on the time zone
  if (!inherits(start_dt, "POSIX")) 
    start_dt <- as.POSIXct(start_dt, tz = tz)
  if (!inherits(end_dt, "POSIX")) 
    end_dt <- as.POSIXct(end_dt, tz = tz)
  
  # Make a list of the things that will be passed to the ed2 in file. 
  ed2in_default <- list(IYEARA = as.numeric(format(start_dt, 
                                                   "%Y")), IMONTHA = as.numeric(format(start_dt, "%m")), 
                        IDATEA = as.numeric(format(start_dt, "%d")), ITIMEA = as.numeric(format(start_dt, 
                                                                                                "%H")), IYEARZ = as.numeric(format(end_dt, "%Y")), 
                        IMONTHZ = as.numeric(format(end_dt, "%m")), IDATEZ = as.numeric(format(end_dt, 
                                                                                               "%d")), ITIMEZ = as.numeric(format(end_dt, "%H")), 
                        FFILOUT = file.path(outdir, "analysis"), SFILOUT = file.path(outdir, "history"))
      
                                                                                     
  # Setting up the configuration xml file                                                                                                                    
  configfile <- NULL
  if (!is.null(configxml)) {
    if (is.character(configxml)) {
      stopifnot(file.exists(configxml))
      configfile <- configxml
    }
    else if (is.list(configxml)) {
      configfile <- file.path(outdir, "config.xml")
      write_configxml(configxml, configfile)
    }
  }
  if (!is.null(configfile)) {
    ed2in_default[["IEDCNFGF"]] <- configfile
  }
  # I am not sure what the modifylist function is doing here, 
  # ah the mofiy list will  unes elements of a list. 
  ed2in_args <- modifyList(ed2in_default, rlang::list2(...))
  
  # This does eomsehting with the ed2 in  file 
  settings <- ed2in(!!!ed2in_args)
  settings_file <- file.path(outdir, "ED2IN")
  
  write_ed2in(settings, settings_file)
  # This is calling the ed2 exe directory the option -f and then the name of the ED2IN 
  processx::process$new(ed2_exe, c("-f", settings_file), wd = wd, 
                        stdout = stdout, stderr = stderr)
#}
#<bytecode: 0x10a8369b0>
#  <environment: namespace:ed4forte>
  
  
  
  # !/bin/bash
  # cd /qfs/people/dorh012/forte-workflow/testing-ensemble/forte-ed-runs/cases/001CTS/
  # /people/dorh012/ed-source-code/ed_2.2-opt ./ED2IN
  