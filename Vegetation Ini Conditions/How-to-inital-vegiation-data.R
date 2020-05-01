# Breaking down the fortedata2ed function 

# The inventory data frame has information  about the trees at each plot (speceis, caoopy, ect.)
inventory = fortedata::fd_inventory()

# But the subpplot has information about the plot, the name, area, and boudries 
subplots = fortedata::fd_subplots() 

# TOBD what this prefix thing does. 
output_prefix = NULL


  if (!"trk" %in% colnames(subplots)) 
    #TODO what does trk mean?
    subplots[["trk"]] <- 3
  if (!"age" %in% colnames(subplots)) 
    # TODO what is the age of the forest 
    # What is the age we are adding the age of the forest stand? Is it really 100 years old? 
    subplots[["age"]] <- 100
  if (!"area" %in% colnames(subplots)) {
    # The fraction of the area that belongs to each treatment plot. 
    subplots[["area"]] <- with(subplots, Subplot_area_m2/sum(Subplot_area_m2))
    
  }
  if (!"patch" %in% colnames(subplots)) {
    # Okay so we are usingthe subplots as patches in the ed set up. 
    subplots[["patch"]] <- with(subplots, paste(Plot, Subplot, 
                                                Replicate, sep = "-"))
  }
# these are the column names for the patch information. 
# TODO try to figure out what is going on with the different abreviasitons
  pss_cols <- c("time", "patch", "trk", "age", "area", "water", "fsc", "stsc",
                "stsl", "ssc", "lai", "msn", "fsn")
  # This is a default patch set up but there are a few columns missing, not really 
  # sure why that is. 
  pss_default <- tibble::tibble(time = -999, water = -999, 
                                lai = -999, fsc = 0.46, stsc = 3.9, stsl = 3.9, ssc = 0.003, 
                                msn = 1, fsn = 1)
  
  # This is setting up for the cohorts with some default values. 
  css_cols <- c("time", "patch", "cohort", "dbh", "hite", "pft", 
                "n", "bdead", "balive", "lai")
  css_default <- tibble::tibble(time = -999, hite = -999, bdead = -999, 
                                balive = -999, lai = -999)
  
  
  patches <- subplots %>% 
    # Add the default values of the missing patch information to the subplots data frame. 
    dplyr::mutate(!!!pss_default[, setdiff(colnames(pss_default), colnames(.))]) %>% 
    # This does something with selecting columns. I think? 
    dplyr::select(!!pss_cols, dplyr::everything())
  
  # For the cohort infomration add patch infomration and the ED PFT
  css <- inventory %>% 
    dplyr::inner_join(patches, c("Replicate", "Plot", "Subplot")) %>% 
    # The species_pfts function is from the ed4forte package and maps the species 
    # name from the inventory to the pft recognized by ED
    dplyr::inner_join(species_pfts(), "Species") %>% 
    dplyr::mutate(cohort = dplyr::row_number(), 
                  n = 1/Subplot_area_m2, 
                  !!!css_default[, setdiff(colnames(css_default), colnames(.))]) %>% 
    dplyr::rename(dbh = DBH_cm) %>% 
    # It is not selecting only the ccs_cols but other things to. Really I don't 
    # understand what is going on here. Am I turning into a bad coder? 
    dplyr::select(!!css_cols)
  
  # This call selects only the patch infomraiton. 
  pss <- patches %>% 
    dplyr::select(!!pss_cols)
  
  # If ths file is not being written out the it is returned as a list and then 
  # is fed into something 
  if (!is.null(output_prefix)) {
    dir.create(dirname(output_prefix), showWarnings = FALSE, 
               recursive = TRUE)
    readr::write_delioutput_prefixm(css, coords_prefix(output_prefix, 
                                          "css"), delim = " ")
    readr::write_delim(pss, coords_prefix(output_prefix, 
                                          "pss"), delim = " ")
  }
  invisible(list(css = css, pss = pss))
}