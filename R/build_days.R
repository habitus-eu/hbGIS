
#' Calculate day-level summaries from the whenwhat dataset
#'
#' @description Build a days dataset by summarising \code{whenwhat}
#' by day and person (\code{identifier}). 
#'
#' @param whenwhatwhere The whenwhatwhere data obtained from \code{\link{build_whenwhatwhere}}.
#' @param verbose Print progress to console. Default is \code{TRUE}.
#' @param where_field ...
#' @param whenwhat_field ...
#' @param loca Nested list with location information
#'
#' @return A table summarised by day.
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom purrr reduce
#'
#' @export
# Code modified from https://thets.github.io/palmsplusr/
build_days <- function(whenwhatwhere = NULL, verbose = TRUE, 
                       where_field = NULL,
                       whenwhat_field = NULL,
                       loca = NULL) {
  # Note:
  # home, school, home_nbh, school_nbh (or similar) need to be present, 
  # because the functions that are passed on assume that they exist
  # So, now we need to create those objects from object loca
  Nlocations = length(loca)
  identifier = NULL
  for (i in 1:Nlocations) {
    for (j in 1:2) {
      txt = paste0(names(loca[[i]])[j], " = loca[[i]][[j]]")
      eval(parse(text = txt))
    }
  }
  # Initialise objects, to avoid R check problems with undeclared objects in dyplr expressions
  duration = datetime = name = is_where_field = NULL
  # filter only where_field rows where is_where_field column is TRUE (sounds redundant, but leaving it in for now)
  where_field <- where_field %>% filter(is_where_field == TRUE)
  where_names <- where_field %>% pull(name) # extract only the names of the where fields
  # add total, because we also want to get the total
  if (is.null(where_names)) {
    where_names <- "total"
  } else {
    where_names <- c("total", where_names)
  }
  where_args <- list(total = 1)
  # Parse and transform text-format formulas from the where_field 
  # into R expressions. So, the formulas do not change they are only made ready
  # for application to the data
  where_args <- c(where_args, setNames(where_field[[2]], where_field[[1]]) %>%
                     lapply(parse_expr))
  whenwhatwhere <- whenwhatwhere %>% # object produced by build_whenwhatwhere
    mutate(!!! where_args) %>% # create new columns that are functions of existing columns by using the where_field formulas
    mutate_if(is.logical, as.integer) # convert logical columns to integer
  
  
  # Take the whenwhat_field names that also have is_where_field = TRUE
  whenwhat_names <- whenwhat_field %>% filter(is_where_field == TRUE) %>% pull(name)
  
  # Note code below assumes that epoch length is constant across the dataset
  # as discussed in https://github.com/habitus-eu/hbGIS/issues/16
  whenwhatwhere <- whenwhatwhere %>%
    st_set_geometry(NULL) %>% # convert sf object to data.table to allow for next steps
    dplyr::select(identifier, datetime, any_of(where_names), all_of(whenwhat_names)) %>% # select only columns needed
    mutate(duration = 1) %>% # add column duration with value 1
    mutate_at(vars(-identifier,-datetime), ~ . * palms_epoch(whenwhatwhere) / 60) %>% # convert time indicators to not reflect number of epocchs but actual time in minutes
    group_by(identifier, date = as.Date(datetime)) %>% # add grouping by recording identifier and new date column
    dplyr::select(-datetime) # remove datetime column
  
  x <- list()
  for (i in where_names) {
    x[[i]] <- whenwhatwhere %>%
      filter(!!(as.name(i)) > 0) %>% #keep non-empty where_names?
      dplyr::select(-any_of(where_names)) %>% # ignore where_names
      summarise_all(~ sum(.)) %>% # sum per grouping levels date and identifier (as specified above)
      ungroup() %>% # remove groups
      rename_at(vars(-identifier, -date), ~ paste0(i, "_", .)) # append where_name to column names as we will have for each where location the same columns
  }
  # Merge all items of list x together
  result <- x %>%
    reduce(left_join, by = c("identifier" = "identifier", "date" = "date"))
  
  # Count the number of segments per day per identifier per where 
  # In other words, how often was a person in a certain location on a certain day.
  segmentcount = function(x) {
    x = as.numeric(unlist(x)) # x is a tibble column, so first convert to numeric vector
    return(length(which(rle(x)$values != 0)))
  }
  # for all where_names:
  for (dom in where_names) { 
    # that are not total:
    if (dom != "total") {
      result[, dom] <- NA
      # for each individual:
      for (id in unique(result$identifier)) { 
        # for each day:
        for (date in as.Date(unique(result$date[which(result$identifier == id)]))) {
          # count umber of segments:
          CNT = segmentcount(whenwhatwhere[which(whenwhatwhere$identifier == id & whenwhatwhere$date == date), dom])
          result[which(result$identifier == id & result$date == date), dom] = CNT
        }
      }
      names(result)[which(names(result) == dom)] = paste0(dom, "_segmentcount")
    }
  }
  return(result)
}
