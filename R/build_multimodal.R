
#' Build multimodal trips from trajectories
#'
#' @description Build multimodal trips from trajectories.
#'
#' @param trajectories The trajectories object built with \code{palms_calc_trajectories}.
#' @param spatial_threshold Spatial threshold in meters
#' @param temporal_threshold Temporal threshold in minutes
#' @param whenwhatwhere The dataset build by \code{build_hbGIS}
#' @param verbose Print progress after each step. Default is \code{TRUE}.
#' @param multimodal_fields ...
#' @param trajectory_locations ...
#'
#'
#'
#' @details Several columns are required in the \code{trajectories} dataset. These
#' need to be added as trajectory fields:
#' \itemize{
#' \item identifier
#' \item tripnumber
#' \item mot
#' \item start
#' \item end
#' \item geometry
#' }
#'
#' @return The input trajectories LINESTRING geometry, collapsed into multimodal trips
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom geosphere distGeo
#' @importFrom data.table rleid
#' @importFrom purrr reduce
#' @importFrom tidyr gather spread unite
#' @importFrom stats setNames
#'
#' @export
# Code modified from https://thets.github.io/palmsplusr/
build_multimodal <- function(trajectories = NULL,
                                 spatial_threshold,
                                 temporal_threshold,
                                 whenwhatwhere = NULL,
                                 verbose = TRUE,
                                 multimodal_fields = NULL,
                                 trajectory_locations = NULL) {
  # Initialise objects, to avoid R check problems with undeclared objects in dyplr expressions
  tripnumber = geometry = start_point = end_point = end_prev = NULL
  triptype = mot = variable = value = start_trip = end_trip = NULL
  distance_diff = time_diff = mmt_number = mmt_criteria = NULL
  identifier = NULL
  if (!all(c("identifier", "tripnumber", "start", "end", "geometry", "mot") %in% colnames(trajectories))) {
    warning("Your trajectories data does not contain the required column names... skipping multimodal analyses")
    df = NULL
  } else {
    
    if (verbose) cat('Calculating multimodal eligibility...')
    
    # Expand trajectories with columns that describe the distance and time to the previous trajectory
    # and use these to determine if a trajectory meets spatial and temporal criteria
    trajectories <- trajectories %>%
      arrange(identifier, tripnumber) %>% # order rows base don identifier and trip number
      mutate(time_diff = difftime(start, lag(end), units = "mins")) %>% # add time_diff (essentially trajectory duration)
      group_by(identifier, tripnumber) %>% # group by recording id and trip number
      mutate(start_point = st_as_text(st_cast(geometry, "POINT")[1]),
             end_point = st_as_text(st_cast(geometry, "POINT")[length(st_cast(geometry, "POINT"))])) %>% # add in character format the locations of the start- and end point
      ungroup() %>%
      mutate(end_prev = lag(end_point, default = start_point[1])) %>% # add column with endpoint of previous trajectory
      group_by(identifier, tripnumber) %>% # group again by recording id and trip number
      mutate(distance_diff = distGeo(
        matrix(c(st_as_sfc(end_prev, crs = 4326)[[1]][1],
                 st_as_sfc(end_prev, crs = 4326)[[1]][2]), ncol = 2),
        matrix(c(st_as_sfc(start_point, crs = 4326)[[1]][1],
                 st_as_sfc(start_point, crs = 4326)[[1]][2]), ncol = 2))) %>% # Calculate distance between previous trajectory and start of current trajectory
      ungroup() %>%
      mutate(mmt_criteria = ((distance_diff < spatial_threshold) & (time_diff < temporal_threshold)),
             mmt_number = NA) # assess for each trajectory whether the distance and time to previous trajectory is below thresholds
    
    # NOTE: Abbreviation mmt standards for multi modal trips, which is confusing
    # as the code also includes trips that are not multimodal.
    
    if (verbose) cat('done\nAssigning trip numbers...')
    
    # Assign temporary trip numbers, if trip involves multiple trajectories then they receive jointly one number
    # The trip number is the numeric time at which the trip starts
    for (i in 1:(nrow(trajectories) - 1)) {
      trajectories$mmt_number[i] <- ifelse(test = (!trajectories$mmt_criteria[i]) & trajectories$mmt_criteria[i + 1],
                                   yes = as.numeric(trajectories$start[i]),
                                   no = ifelse(test = trajectories$mmt_criteria[i],
                                               yes = trajectories$mmt_number[i - 1],
                                               no = as.numeric(trajectories$start[i])))
    }
    trajectories$mmt_number[nrow(trajectories)] <- ifelse(test = trajectories$mmt_criteria[nrow(trajectories)],
                                          yes = trajectories$mmt_number[nrow(trajectories) - 1],
                                          no = as.numeric(trajectories$start[nrow(trajectories)]))
    
    # Assign final trip number with run-length encoding
    trajectories <- trajectories %>%
      group_by(identifier) %>%
      mutate(mmt_number = data.table::rleid(mmt_number)) %>% # Use run-length encoding to assign mmt numbers
      ungroup() %>%
      dplyr::select(!any_of(c(start_point, end_point, end_prev, mmt_criteria, time_diff, distance_diff))) # drop columns that are no longer needed
    
    if (verbose) cat('done\nCalculating variables...')
    #------------------------------------------------------------
    # Split trajectory variables per mot
    mot_split <- trajectories %>%
      dplyr::select(any_of(c("mot", "mmt_number", "identifier", "geometry", multimodal_fields$name))) %>%
      mutate(mot = paste0("mot_", mot)) %>% # convert mot-column to character "mot_motcode"
      gather(variable, value, -mmt_number, -mot, -identifier, -geometry) %>% # reshape data to long format with only the -listed variables in columns
      unite(col, mot, variable) %>% # combine mot and variable column into a new column mot, e.g. mot_3_duration
      spread(col, value) %>% # reshape data back to wide based on col column
      arrange(identifier, mmt_number) %>% # order data based on recording id and mmt trip number
      cbind(trajectories) %>% #turn simple features tibble into simple features data.frame
      dplyr::select(!any_of(ends_with(".1"))) # omit variables that end with .1
    
    # Derive df_fields, which has the mmt characteristics such as speed,
    # movement intensity and duration per mode of transport.
    # We does this by applying the multimodal_fields formulas, 
    # e.g. typically sum() for time variables and mean() for speed
    
    df_fields <- list()
    
    for (i in unique(multimodal_fields$formula)) {
      df_fields[[i]] <- mot_split %>%
        as.data.frame() %>%
        group_by(identifier, mmt_number) %>%
        summarise_at(vars(matches(
          paste(multimodal_fields$name[multimodal_fields$formula == i], collapse = "|"))),
          i, na.rm = TRUE)
    }
    # collapse df_fields that is a list into a grouped tible
    df_fields <- reduce(df_fields, left_join,
                        by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))
    
    df_fields[is.na(df_fields)] <- NA
    
    # Get unique locations related to trajectories
    trajectory_location_names <- unique(c(trajectory_locations$start_criteria,
                      trajectory_locations$end_criteria))
    
    # Rather than recalculating geometry for these locations, just lookup in whenwhatwhere:
    lookup <- whenwhatwhere %>%
      filter(tripnumber > 0 & triptype %in% c(1, 4)) %>% # start- and end-point of each trip
      as.data.frame() %>%
      dplyr::select(all_of(c("identifier", "tripnumber", "triptype", trajectory_location_names)))
    # Helper function to lookup start and end locations from the lookup table
    lookup_locations <- function(identifier, start_trip, start_loc, end_trip, end_loc, lookup) {
      n1 <- lookup[(lookup$identifier == identifier) & (lookup$tripnumber == start_trip) & (lookup$triptype == 1), start_loc]
      n2 <- lookup[(lookup$identifier == identifier) & (lookup$tripnumber == end_trip) & (lookup$triptype == 4), end_loc]
      return(n1 & n2)
    }
    # Parse and transform text-format formulas into R expressions. 
    args_locations <- setNames(
      paste0("lookup_locations(identifier, start_trip, '",
             trajectory_locations$start_criteria, "', end_trip, '", 
             trajectory_locations$end_criteria, "', lookup)"),
      trajectory_locations$name) %>%
      lapply(parse_expr)
    #------------------------------------------------------------
    # Derive df_other, which has the mmt trip locations and basic description:
    df_other <- mot_split %>%
      group_by(identifier, mmt_number) %>% # group by recording id and (multimodal)-trip
      summarise(start_trip = first(tripnumber), # summarise at group level based on these 8 variables
                end_trip = last(tripnumber),
                trip_numbers = paste0(tripnumber, collapse = ">"), 
                n_segments = n(),
                mot_order = paste0(mot, collapse = ">"),
                start = first(start),
                end = last(end),
                do_union = FALSE, .groups = 'keep') %>%
      rowwise() %>% # prepare row by row application of next step
      mutate(!!!args_locations) %>% # apply formula to lookup the locations for the give start and end times
      ungroup() %>%
      dplyr::select(!any_of(c(start_trip, end_trip))) %>% # remove start_trip and end_trip numbers
      mutate_if(is.logical, as.integer) # convert logical columns to integer
    #------------------------------------------------------------
    # Combine
    if (exists("df_fields")) {
      df <- reduce(list(df_other, df_fields), left_join, by = c("identifier" = "identifier", "mmt_number" = "mmt_number"))
    } else {
      df <- df_other
    }
    if (verbose) cat('done\n')
  }
  return(df)
}
