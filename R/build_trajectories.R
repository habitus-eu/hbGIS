
#' Build trajectory (trip?) summaries from the whenwhatwhere output
#'
#' @description Build trip summaries based on the \code{build_whenwhatwhere} dataset. This
#' returns a \code{sf data.frame} with \code{LINESTRING} geometry. Three columns
#' are returned by default (\code{identifier}, \code{tripnumber}, and \code{geometry}).
#'
#' @param whenwhatwhere The whenwhatwhere object create by \code{build_whenwhatwhere}.
#' @param trajectory_fields trajectory_fields
#' @param trajectory_locations trajectory_locations
#'
#' @return A table of individual trips represented as \code{LINESTRING} geometry.
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#'
#' @export
# Code modified from https://thets.github.io/palmsplusr/
build_trajectories <- function(whenwhatwhere = NULL, trajectory_fields = NULL, trajectory_locations = NULL) {
  # Initialise objects, to avoid R check problems with undeclared objects in dyplr expressions
  name = after_conversion = tripnumber = identifier = NULL
  
  # Split formulas from trajectory_fields:
  # Formulas to be applied to time series where geometry is still POINT (one row is one timepoint)
  point_formulas <- trajectory_fields %>% filter(after_conversion == FALSE)
  # Formulas to be applied per LINESTRING (one row is one trajectory)
  line_formulas <- trajectory_fields %>% filter(after_conversion == TRUE)
  
  # Parse and transform text-format formulas into R expressions. 
  # So, the formulas do not change they are only made ready
  # for application to the data
  
  # ... for the trajectory_fields
  point_formulas <- setNames(point_formulas$formula, point_formulas$name) %>% lapply(parse_expr)
  line_formulas <- setNames(line_formulas$formula, line_formulas$name) %>% lapply(parse_expr)
  
  if (length(trajectory_locations) > 0) {
    # ... for the trajectory_locations
    formulas_locations <- setNames(paste0("first(", trajectory_locations$start_criteria,
                                      ") & last(", trajectory_locations$end_criteria, ")"),
                               trajectory_locations$name) %>% lapply(parse_expr)
    formulas_locations = formulas_locations[order(names(formulas_locations))]
    # Combine with point_formulas subset of the trajectory_fields
    point_formulas <- c(point_formulas, formulas_locations)
  }
  # Build trajectories object
  trajectories <- whenwhatwhere %>% #whenwhatwhere field produced by build_whenwhatwhere
    filter(tripnumber > 0) %>% # only data points that are part of a trip
    group_by(identifier, tripnumber) %>% # group by identifier and tripnumber
    summarise(!!!point_formulas, do_union = FALSE, .groups = 'keep') %>% # summarise with 'before' formulas from trajectory_fields
    st_cast("LINESTRING") %>% # cast the geometry from POINT to LINESTRING
    mutate(!!!line_formulas) %>% # create new columns based on the 'after' formulas from trajectory_fields
    ungroup() %>%
    mutate_if(is.logical, as.integer) # convert logical columns to integer
  
  return(trajectories)
}





