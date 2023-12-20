#' hbGIS
#'
#' @param gisdir Path to directory with GIS files
#' @param palmsdir Path to hbGPS output directory
#' @param gislinkfile Path to participant basis file, which is the file that links all participant identifies with the GIS data
#' @param outputdir Path to outputdir location
#' @param dataset_name Name of dataset
#' @param configfile Configuration file
#' @param verbose verbose Boolean
#' @param baselocation character, to specify reference location for individuals, e.g. home
#' @param groupinglocation character, to specify reference location location for groups, e.g. school
#' @param write_shp boolean, to indicate whether shape file should be written
#' @param split_GIS boolean, to indicate whether sublocation inside GIS files are to be split
#' @param sublocationID character, GIS column name to be used as identifier for sublocations
#' @return palms_to_clean_lower object
#' @importFrom stats end start formula as.formula
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv read_csv
#' @import palmsplusr
#' @import dplyr
#' @import lwgeom
#' @importFrom utils head tail
#' 
#' @export

hbGIS <- function(gisdir = "",
                  palmsdir = "",
                  gislinkfile = "",
                  outputdir = "",
                  dataset_name = "",
                  configfile = NULL,
                  verbose = TRUE,
                  baselocation = NULL,
                  groupinglocation = NULL,
                  write_shp = NULL,
                  split_GIS = NULL,
                  sublocationID = NULL) {
  
  #===============================================
  # Load configuration and define field tables
  #===============================================
  if (length(configfile) > 0) {
    # check for missing parameters, such that hbGIS can fall back on defaults
    # here the config_pamsplusr file inside the package is assumed to hold all the defaults.
    config_def  = system.file("testfiles_hbGIS/config_hbGIS.csv", package = "hbGIS")[1]
    
    params_def = load_params(file = config_def)
    params_def$id = rownames(params_def)
    params = load_params(file = configfile)
    params$id = rownames(params)
    missingPar = which(params_def$id %in% params$id == FALSE)
    if (length(missingPar) > 0) {
      # update the configfile as provide by the user
      params = rbind(params, params_def[missingPar,])
      params = params[, -which(colnames(params) == "id")]
      update_params(new_params = params, file = configfile)
    }
    rm(params_def)
    config <- configfile
  } else {
    # If no configfile is provided fall back on default
    config <- system.file("testfiles_hbGIS/config_hbGIS.csv", package = "hbGIS")[1]
  }
  # adding fields
  CONF = read.csv(config, sep = ",")
  CONF$start_criteria = ""
  CONF$end_criteria = ""

  # Extract general parameters from config file if not provided as input arguments
  if (is.null(groupinglocation)) {
    groupinglocation =  CONF$formula[which(CONF$name == "groupinglocation")]
  }
  if (is.null(baselocation)) {
    baselocation = CONF$formula[which(CONF$name == "baselocation")]
  }
  if (is.null(write_shp)) {
    write_shp = CONF$formula[which(CONF$name == "write_shp")] # whether to wrist a shape file
  }
  if (is.null(split_GIS)) {
    split_GIS = CONF$formula[which(CONF$name == "split_GIS")] # whether to split sublocations (TRUE) or union them
  }
  if (is.null(sublocationID)) {
    sublocationID = CONF$formula[which(CONF$name == "sublocationID")]  # column name in GIS file to identify sublocation id
  }
  CONF = CONF[-which(CONF$context == "general"),]

  #------------------------------------------------------------
  lon = identifier = palms = NULL # . = was also included, but probably wrong
  
  
  
  
  #===============================================
  # GIS files
  #===============================================
  # create list structure to house the location objects
  shapefilenames = dir(path = gisdir, full.names = FALSE, pattern = "[.]shp")
  locationNames = unique(gsub(pattern = "table|_|buffers|[.]|xml|shp|loc", replacement = "", x = shapefilenames))
  
  Nlocations = length(locationNames)
  loca = vector("list", Nlocations)
  names(loca) = locationNames
  # Initialise loca object which is a list that for each location
  # has a list with four items:
  # 1 - location shape
  # 2 - location neighbourhood shape
  # 3 - shape file to read 1
  # 4 - shape file to read 2
  # Sometimes neighbourhood is referred to as buffer
  # For some location types such as parks some of these are missing,
  # which for now is ok, those lists will be left empty.
  for (i in 1:Nlocations) {
    loca[[i]] = vector("list", 4)
    names(loca[[i]]) =  c(locationNames[i], paste0(locationNames[i], "_nbh"), 
                          paste0(locationNames[i], "_tablefile"),
                          paste0(locationNames[i], "_locbufferfile"))
  }
  
  # Helper function to find shape files
  find_file = function(path, namelowercase) {
    allshpfiles = dir(path, recursive = TRUE, full.names = TRUE, pattern = "shp")
    file_of_interest = allshpfiles[which(tolower(basename(allshpfiles)) == namelowercase)]
    if (length(file_of_interest) == 0) file_of_interest = NULL
    return(file_of_interest)
  }
  # Find shape files
  locationNames_nbh = locationNames_table = NULL
  ignore4trajectories = NULL
  for (jj in 1:Nlocations) {
    # files with _table at the end of their name indicate that it is
    # a location for which entries exist in the GIS-Person linkage file
    findfile3 = find_file(path = gisdir, namelowercase = paste0(locationNames[jj], "_table.shp"))
    publiclocation = ifelse(test = is.null(findfile3), yes = TRUE, no = FALSE)
    if (publiclocation == FALSE) {
      loca[[jj]][3] = findfile3
      loca[[jj]][[1]] = sf::read_sf(loca[[jj]][3]) #e.g. school or home
      locationNames_table = c(locationNames_table, locationNames[jj])
    }
    # files with "loc_" and "buffers" in their name are assumed to be the areas surrounding
    # the table files (if exist)
    findfile4 = find_file(path = gisdir, namelowercase = paste0("loc_", locationNames[jj], "buffers.shp"))
    if (!is.null(findfile4)) {
      loca[[jj]][4] = findfile4
      locationNames_nbh = c(locationNames_nbh, locationNames[jj])
    } else {
      # If it is not a buffer file then it is space file, eg. public park
      findfile4 = find_file(path = gisdir, namelowercase = paste0("loc_", locationNames[jj], ".shp"))
      if (!is.null(findfile4)) {
        loca[[jj]][4] = findfile4
        locationNames_nbh = c(locationNames_nbh, locationNames[jj])
      }
    }
    # Only load shape file if it exists
    if (!is.null(loca[[jj]][4][[1]])) {
      shp_dat = sf::read_sf(loca[[jj]][4]) #e.g. school_nbh, home_nbh, or park
      # Check whether there are multiple polygons in the shapefile:
      nshp = nrow(shp_dat)
      loca[[jj]][[2]] = shp_dat # look at all sublocations combined either way
      if (publiclocation == TRUE & verbose == TRUE) {
        cat(paste0("\n", basename(as.character(unlist(loca[[jj]][4]))),
                   " => ", paste0(names(shp_dat), collapse = ", "), " (", nshp, " geoms)"))
      }
      if (nshp > 1 & split_GIS == TRUE & sublocationID %in% names(shp_dat) & publiclocation) {
        collect_na = NULL
        # Treat each polygon as a separate location
        for (gi in 1:nshp) {
          fn_4 = as.character(unlist(loca[[jj]][4]))
          gi2 = Nlocations + gi
          loca[[gi2]] = vector("list", 4)
          
          objectname = as.character(sf::st_drop_geometry(shp_dat[gi, sublocationID]))
          if (objectname == "NA") collect_na = c(collect_na, gi)
          loca[[gi2]][[2]] = shp_dat[gi, ]
          loca[[gi2]][[4]] = fn_4
          newname = paste0(locationNames[jj], objectname)
          names(loca[[gi2]]) =  c(newname,
                                  paste0(newname, "_nbh"), 
                                  paste0(newname, "_tablefile"),
                                  paste0(newname, "_locbufferfile"))
          locationNames = c(locationNames, newname)
          names(loca)[[gi2]] = newname
          locationNames_nbh = c(locationNames_nbh, newname)
          ignore4trajectories = c(ignore4trajectories, newname)
        }
        if (length(collect_na) > 0) {
          shp_dat = shp_dat[-collect_na, ]
          locationNames = locationNames[-collect_na]
          loca = loca[-collect_na]
        }
      }
    }
  }
  Nlocations = length(locationNames)
  locationNames = names(loca)
  # Force id numbers to be character to standardise format:
  for (i in 1:length(loca)) {
    if (!is.null(loca[[i]][3][[1]])) { # only consider locations with data
      for (j in 1:2) {
        loc_id = grep(pattern = "ID|identifier|_id", x = colnames(loca[[i]][j][[1]]))
        if (length(loc_id) > 0) {
          loca[[i]][j][[1]][[loc_id]] = as.character(loca[[i]][j][[1]][[loc_id]])
        }
      }
    }
  }
  #===============================================
  # hbGPS output (PALMS output)
  #===============================================
  outputFolder = paste0(outputdir, "/hbGIS_output")
  if (!dir.exists(outputFolder)) {
    if (verbose) cat("\nCreating hbGIS output directory\n")
    dir.create(outputFolder)
  }
  sf::sf_use_s2(FALSE)
  # identify palms csv output files in palmsdir:
  if (!is.null(palmsdir)) {
    palms_country_files <- list.files(path = palmsdir, pattern = "*.csv", full.names = TRUE)
    # skip the combined file that palms generates
    palms_country_files = grep(pattern = "combined.csv", x = palms_country_files, invert = TRUE, value = TRUE)
  } else {
    palms_country_files = NULL
  }
  if (length(palms_country_files) == 0) {
    # Simulate hbGPS output (only for code developement purposes)
    Nmin = 500
    now = as.POSIXct("2023-11-30 10:00:00 CET")
    dateTime = seq(now, now + ((Nmin - 1) * 60), by = 60)
    example_object = loca[[1]][[2]][1,]
    # in next link lwgeom is a dependency but sf somehow does not load it
    if (!requireNamespace("lwgeom", quietly = TRUE)) {
      stop(
        "The 'lwgeom' package is required to run this function. ",
        "Please install it first."
      )
    }
    point_in_object = suppressMessages(sf::st_sample(x = example_object, size = 1))
    
    xy = sf::st_coordinates(x = point_in_object)
    
    # latitude is for most of the time 1 lat degree away from location
    # but for 30 minutes inside the location surround by 5 minute trips before and after
    trip = seq(xy[1] - 1, xy[1] - 0.2, by = 0.2)
    away = rep(xy[1] - 1, (Nmin/2) - 20)
    lon = c(away, trip, rep(xy[1], 30), rev(trip), away)
    lat = rep(xy[2], Nmin) # lon stays the same the entire time
    tripNumber = c(rep(0, length(away)), rep(1, 5), rep(0, 30), rep(2, 5), rep(0, length(away)))
    sedentaryBoutNumber = c(rep(1, length(away)), rep(0, 5), rep(2, 30), rep(0, 5), rep(3, length(away)))
    tripType = rep(0, Nmin)
    tripMOT = rep(0, Nmin)
    tripType[which(diff(tripNumber) > 0) + 1] = 1
    tripType[which(diff(tripNumber) < 0)] = 4
    tripMOT[which(tripNumber != 0)] = 3
    hbGPSout = data.frame(identifier = "sim1",
                          dateTime	= dateTime,
                          dow = rep(5, Nmin),
                          lat	= lat,
                          lon	= lon,
                          fixTypeCode	= rep(-1, Nmin),
                          iov	= rep(2, Nmin), # all the time outdoor (indoor, outdoor, vehicle)
                          tripNumber = tripNumber,
                          tripType = tripType,
                          tripMOT	= tripMOT,
                          activity = rep(0, Nmin),
                          activityIntensity = rep(0, Nmin),
                          activityBoutNumber = rep(0, Nmin),
                          sedentaryBoutNumber = sedentaryBoutNumber)
    # hbGPSout <- st_as_sf(hbGPSout, coords = c("lon", "lat"), crs = 4326)
    if (is.null(palmsdir)) palmsdir = outputdir
    if (!dir.exists(palmsdir)) dir.create(palmsdir, recursive = TRUE)
    palms_country_files = paste0(palmsdir, "/combined.csv")
    write.csv(hbGPSout, file = palms_country_files, row.names = FALSE)
  }
  
  # read and combine palms csv output files 
  csv_palms <- lapply(palms_country_files, FUN = readr::read_csv, col_types = list(
    identifier = readr::col_character(),
    dow = readr::col_integer(),
    lat = readr::col_double(),
    lon = readr::col_double(),
    fixTypeCode = readr::col_integer(),
    iov = readr::col_integer(),
    tripNumber = readr::col_integer(),
    tripType = readr::col_integer(),
    tripMOT = readr::col_integer(),
    activity = readr::col_double()
  ), show_col_types = FALSE)
  PALMS_combined <- bind_rows(csv_palms)
  # Data cleaning:
  PALMS_reduced <- subset(PALMS_combined, lon > -180)
  palms_reduced_cleaned <- check_and_clean_palms_data(PALMS_reduced, dataset_name, outputdir)
  PALMS_reduced$dateTime = as.POSIXct(PALMS_reduced$dateTime, format = "%d/%m/%Y %H:%M:%S", tz = "")
  
  # Write to csv and read using read_palms to format the object as expected from the rest of the code
  # if (substring(text = outputFolder, first = 1, last = 1) == ".") {
  #   print("convert")
  #   print(outputFolder)
  #   outputFolder = gsub(pattern = "[.]", replacement = getwd(), x = outputFolder)
  #   print(outputFolder)
  # }
  PALMS_reduced_file = suppressWarnings(normalizePath(paste0(outputFolder, "/", stringr::str_interp("PALMS_${dataset_name}_reduced.csv"))))
  # if (verbose) cat(paste0("\nCheck PALMS_reduced_file: ", PALMS_reduced_file))
  write.csv(palms_reduced_cleaned, PALMS_reduced_file, row.names = FALSE)
  palms = palmsplusr::read_palms(PALMS_reduced_file, verbose = FALSE)
  palms$datetime = as.POSIXct(palms$datetime, format = "%d/%m/%Y %H:%M:%S", tz = "")
  
  
  #=====================================================
  # Expand CONF with standard location based fields
  #=====================================================
  if (verbose) cat("\n<<< expand CONF...\n")
  
  # where_field:
  #-------------------
  # # ignore stored definition as we no longer use this
  # CONF = CONF[which(CONF[,1] != "where_field"), ] 
  element3 = ifelse(length(locationNames_table) > 0, yes = paste0("!", paste0("at_", locationNames_table, collapse = " & !"), " & "), no = "")
  CONF[nrow(CONF) + 1, ] = c("where_field",
                             "transport",
                             paste0(element3, "(pedestrian | bicycle | vehicle)"),
                             TRUE, NA, "", "")
  CONF[nrow(CONF) + 1, ] = c("where_field",
                             "other",
                             paste0(element3, "(!pedestrian & !bicycle & !vehicle)", # removed because theorectically possible
                                    ifelse(test = length(locationNames_nbh) > 0, yes = " & ", no = ""),
                                    paste0("!", paste0("at_", locationNames_nbh, "_nbh", collapse = " & !"))),
                             TRUE, NA, "", "")
  
  cnt = nrow(CONF)
  CONF[cnt + Nlocations,] = NA
  cnt = cnt + 1
  
  for (i in 1:Nlocations) {
    # where_field:
    #-------------------
    if (locationNames[i] %in% locationNames_table) {
      CONF[cnt, ] = c("where_field",
                      locationNames[i],
                      paste0("at_", locationNames[i]), 
                      TRUE,
                      NA, "", "")
    } else if (locationNames[i] %in% locationNames_nbh) {
      # condition that only needs to be used if table element is present
      at_table = ifelse(test = locationNames[i] %in% locationNames_table == TRUE, yes = paste0("!at_", locationNames[i], " &"), no = "")
      CONF[cnt, ] = c("where_field",
                      paste0(locationNames[i], "_nbh"),
                      paste0(at_table, " at_", locationNames[i], "_nbh", 
                             " & (!vehicle)"), # removed !pedestrian & !bicycle &  because unclear why these are not possible in a neighbourhood, e.g. park
                      TRUE,
                      NA, "", "")
    }
    cnt = cnt + 1
    # whenwhat_field:
    #-------------------
    if (!is.null(loca[[i]][[1]])) {
      # only do this if there is table data (meaning that location is linked to participant basis file)
      if (locationNames[i] == "home") {
        CONF[cnt, ] = c("whenwhat_field",
                        paste0("at_", locationNames[i]), 
                        paste0("palms_in_polygon(datai, polygons = dplyr::filter(", 
                               locationNames[i],", identifier == i), identifier)"),
                        NA, "", "", "")
        cnt = cnt + 1
        CONF[cnt, ] = c("whenwhat_field",
                        paste0("at_", locationNames[i], "_nbh"), 
                        paste0("palms_in_polygon(datai, polygons = dplyr::filter(",
                               locationNames[i], "_nbh, identifier == i), identifier)"),
                        NA, "", "", "")
        cnt = cnt + 1
      } else {
        CONF[cnt, ] = c("whenwhat_field",
                        paste0("at_", locationNames[i]), 
                        paste0("palms_in_polygon(datai, polygons = dplyr::filter(", 
                               locationNames[i],",", locationNames[i],
                               "_id == participant_basis %>% filter(identifier == i) %>% pull(",
                               locationNames[i], "_id)))"),
                        NA, "", "", "")
        cnt = cnt + 1
        CONF[cnt, ] = c("whenwhat_field",
                        paste0("at_", locationNames[i], "_nbh"), 
                        paste0("palms_in_polygon(datai, polygons = dplyr::filter(", 
                               locationNames[i], "_nbh,", locationNames[i],
                               "_id == participant_basis %>% filter(identifier == i) %>% pull(",
                               locationNames[i], "_id)))"),
                        NA, "", "", "")
        cnt = cnt + 1
      }
    } else {
      # locations not in linkagefile
      # note that colSums will ensure that sublocation are combined
      CONF[cnt, ] = c("whenwhat_field",
                      paste0("at_", locationNames[i], "_nbh"), 
                      paste0("suppressMessages(colSums(sf::st_contains(", locationNames[i], "_nbh, datai, sparse = FALSE)))"),
                      NA, "", "", "")
      cnt = cnt + 1
    }
    reference_location = ifelse("home" %in% locationNames, yes = "home", no = locationNames[1])
    for (j in 1:Nlocations) {
      # Do not derive trajectories for sublocation combinations
      if (all(locationNames[c(i, j)] %in% ignore4trajectories == FALSE)) {
        #add nbh if table is missing
        element1 = ifelse(test = is.null(loca[[i]][[1]]), yes = "_nbh", no = "")
        element2 = ifelse(test = is.null(loca[[j]][[1]]), yes = "_nbh", no = "")
        element4 = ifelse(test = is.null(loca[[reference_location]][[1]]), yes = "_nbh", no = "")
        # trajectory_location:
        #-------------------
        CONF[cnt, ] = c("trajectory_location",
                        paste0(locationNames[i], element1, "_", locationNames[j], element2),
                        paste0("at_", locationNames[i]), NA,
                        paste0("at_", reference_location, element4),
                        paste0("at_", locationNames[i], element1),
                        paste0("at_", locationNames[j], element2))
        cnt = cnt + 1
      }
    }
    CONF = CONF[!duplicated(CONF),]
  }
  if (verbose) cat(">>>\n\n")
  whenwhat_field_rows = which(CONF$context == "whenwhat_field")
  whenwhat_field = tibble(name = CONF$name[whenwhat_field_rows],
                            formula = CONF$formula[whenwhat_field_rows],
                            is_where_field = CONF$is_where_field[whenwhat_field_rows])
  
  where_field_rows = which(CONF$context == "where_field")
  where_field = tibble(name = CONF$name[where_field_rows],
                             formula = CONF$formula[where_field_rows],
                             is_where_field = CONF$is_where_field[where_field_rows])
  #=============================
  # trajectory_fields
  trajectory_field_rows = which(CONF$context == "trajectory_field")
  trajectory_fields = tibble(name = CONF$name[trajectory_field_rows],
                             formula = CONF$formula[trajectory_field_rows],
                             after_conversion = CONF$after_conversion[trajectory_field_rows])
  #=============================
  # multimodal_fields
  multimodal_fields_rows = which(CONF$context == "multimodal_field")
  multimodal_fields = tibble(name = CONF$name[multimodal_fields_rows],
                             formula = CONF$formula[multimodal_fields_rows])
  #=============================
  # trajectory locations
  trajectory_location_rows = which(CONF$context == "trajectory_location")
  trajectory_locations = tibble(name = CONF$name[trajectory_location_rows],
                                start_criteria = CONF$start_criteria[trajectory_location_rows],
                                end_criteria = CONF$end_criteria[trajectory_location_rows])
  write.csv(x = CONF[order(CONF$context), ], file = paste0(outputFolder, "/formula_log.csv"), row.names = FALSE)
  #===============================================
  # Load linkage file and identify which PALMS ids and home/school
  # ids are missing, but allow for publiclocations that are not linked
  # to an ID
  #===============================================
  if (length(gislinkfile) > 0) {
    participant_basis = read_csv(gislinkfile, show_col_types = FALSE)
    
    
    # Check for missing IDs -------------------------------------------------------------------------
    withoutMissingId = check_missing_id(participant_basis, outputFolder, dataset_name, palms,
                                        loca, groupinglocation = groupinglocation,
                                        verbose = verbose)
    palms = withoutMissingId$palms
    participant_basis = withoutMissingId$participant_basis
    loca = withoutMissingId$loca
    write.csv(participant_basis, paste0(outputFolder, "/", stringr::str_interp("participant_basis_${dataset_name}.csv"))) # store file for logging purposes only
    if (length(participant_basis) == 0 || nrow(participant_basis) == 0) {
      stop("\nParticipant basis file does not include references for the expected recording IDs")
    }
  } else {
    participant_basis = ""
  }
  
  
  # Run palmsplusr ----------------------------------------------------------
  fns = c(paste0(outputFolder, "/", dataset_name, "_whenwhatwhere.csv"),
          paste0(outputFolder, "/", dataset_name, "_days.csv"),
          paste0(outputFolder, "/", dataset_name, "_trajectories.csv"),
          paste0(outputFolder, "/", dataset_name, "_multimodal.csv"))
  for (fn in fns) {
    if (file.exists(fn)) file.remove(fn)
  }
  Nlocation_objects = NULL
  for (i in 1:Nlocations) {
    Nlocation_objects = c(Nlocation_objects, length(loca[[i]][[2]])) # at least a nbh object is expected #length(loca[[i]][[1]]), 
  }
  if (verbose) cat("\n<<< building whenwhatwhere...\n")
  if (length(palms) > 0 & length(whenwhat_field) &
      all(Nlocation_objects > 0) & length(participant_basis) > 0) {
    
    whenwhat <- build_whenwhatwhere(data = palms, 
                             whenwhat_field = whenwhat_field,
                             loca = loca,
                             participant_basis = participant_basis,
                             verbose = verbose)
    write_csv(whenwhat, file = fns[1])
    if (verbose) cat(">>>\n")
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  if (verbose) cat("\n<<< building days...")
  if (length(whenwhat) > 0 & length(where_field) > 0 & length(whenwhat_field) &
      all(Nlocation_objects > 0) & length(participant_basis) > 0) {
    days <- build_days(data = whenwhat,
                       where_field = where_field,
                       whenwhat_field = whenwhat_field,
                       loca = loca,
                       participant_basis = participant_basis,
                       verbose = verbose)
    if (length(days) > 0) {
      if (verbose) cat(paste0("\n  N rows in days object: ", nrow(days)))
      write_csv(x = days, file = fns[2])
    } else {
      if (verbose) cat(paste0("\n  WARNING: no days object produced."))
    }
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  if (verbose) cat(">>>\n")
  trajectory_locations = trajectory_locations[order(trajectory_locations$name),]
  
  if (verbose) cat("\n<<< building trajectories...\n")
  if (length(whenwhat) > 0 & length(trajectory_fields) > 0) {
    trajectories <- build_trajectories(data = whenwhat,
                                       trajectory_fields = trajectory_fields,
                                       trajectory_locations = trajectory_locations)
    
    # library(mapview)
    # mapview(list(trajectories, loca$green$green_nbh), col.regions=list("red","blue"), col = list("red", "blue"),
    #         layer.name = c("trajectories", "green spaces"))
    
    # browser()
    if (length(trajectories) > 0) {
      write_csv(trajectories,  file = fns[3])
      shp_file = paste0(outputFolder, "/", dataset_name, "_trajectories.shp")
      if (write_shp == TRUE) {
        if (file.exists(shp_file)) file.remove(shp_file) # remove because st_write does not know how to overwrite
        sf::st_write(obj = trajectories, dsn = shp_file)
      }
      if (verbose) cat(paste0("  N rows in trajectories object: ", nrow(trajectories)))
    } else {
      if (verbose) cat(paste0("  WARNING: no trajectories object produced."))
    }
    if (verbose) cat(">>>\n")
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  if (verbose) cat("\n<<< building multimodal...\n")
  if (length(whenwhat) > 0 & length(multimodal_fields) > 0 & length(trajectory_locations) > 0) {
    multimodal <- build_multimodal(data = trajectories,
                                   spatial_threshold = 200,
                                   temporal_threshold = 10,
                                   whenwhat = whenwhat,
                                   multimodal_fields = multimodal_fields,
                                   trajectory_locations = trajectory_locations,
                                   verbose = verbose)
    if (length(multimodal) > 0) {
      write_csv(multimodal, file = fns[4])
      shp_file = paste0(outputFolder, "/", dataset_name, "_multimodal.shp")
      if (write_shp == TRUE) {
        if (file.exists(shp_file)) file.remove(shp_file) # remove because st_write does not know how to overwrite
        sf::st_write(obj = multimodal, dsn = shp_file)
      }
      if (verbose) cat(paste0("  N rows in multimodal object: ", nrow(multimodal)))
    } else {
      if (verbose) cat(paste0("  WARNING: no multimodal object produced."))
    }
    if (verbose) cat(">>>\n\n")
  } else {
    if (verbose) cat("skipped because insufficient input data>>>\n")
  }
  return()
}
