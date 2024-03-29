#' build_whenwhatwhere
#'
#' @description Build the whenwhat dataset by adding additional columns to the
#'  hbGPS output data based on whenwhat_field.
#'
#' @param data The hbGPS data obtained using \code{read_palms} from palmplusr.
#' @param verbose Print progress to console after each iteration. Default is \code{TRUE}.
#' @param whenwhat_field whenwhat_field defined in hbGIS
#' @param loca Nested list with location information
#' @param participant_basis participant_basis
#'
#' @import dplyr
#' @import sf
#' @import palmsplusr
#' @importFrom rlang parse_expr
#' @importFrom stats setNames
#' @importFrom data.table rbindlist
#'
#' @export
#' 
# Code modified from https://thets.github.io/palmsplusr/
build_whenwhatwhere <- function(data = NULL, verbose = TRUE, whenwhat_field = NULL,
                                loca = NULL,
                                participant_basis = NULL) {
  # Note:
  # home, school, home_nbh, school_nbh (or similar) need to be present, 
  # because the functions that are passed on assume that they exist in the current scope
  # So, create those objects from object loca
  identifier = NULL
  Nlocations = length(loca)
  for (i in 1:Nlocations) {
    for (j in 1:2) {
      txt = paste0(names(loca[[i]])[j], " = loca[[i]][[j]]")
      eval(parse(text = txt))
    }
  }
  # Parse and transform text-format formulas from the whenwhat_field 
  # into R expressions. So, the formulas do not change they are only made ready
  # for application to the data
  field_args <- setNames(whenwhat_field$formula, whenwhat_field$name) %>%
    lapply(parse_expr)
  
  x <- list()
  unique_ids = unique(data$identifier)
  Nids <- length(unique_ids)
  j <- 1 # Object i is expected by the formulas to represent an id, which is why we need object j here.
  # Loop over recordings
  for (i in unique_ids) {
    datai = data %>% # from hbGPS (or PALMS) output data
      filter(identifier == i) # select one recordingid
    x[[i]] <- datai %>% 
      mutate(!!! field_args) %>% # create new columns that are functions of existing columns by using the whenwhat_field formulas
      mutate_if(is.logical, as.integer) # convert logical columns to integer
    if (verbose) {
      cat("[", j, "/", Nids, "] Computed whenwhatwhere for: ", i, "\n", sep = "")
      j <- j + 1
    }
  }
  data <- rbindlist(x) %>% # Combine all list items into a data.table again
    st_set_geometry(data$geometry) # convert to sf (simple features) object with a set geometry
  return(data)
}
