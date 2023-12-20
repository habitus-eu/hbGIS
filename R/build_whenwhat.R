#' build_whenwhat
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
#'
#' @export
#' 
# Code modified from https://thets.github.io/palmsplusr/
build_whenwhat <- function(data = NULL, verbose = TRUE, whenwhat_field = NULL,
                                loca = NULL,
                                participant_basis = NULL) {
  # Note:
  # home, school, home_nbh, school_nbh (or similar) need to be present, 
  # because the functions that are passed on assume that they exist
  # So, create those objects from object loca
  identifier = NULL
  Nlocations = length(loca)
  for (i in 1:Nlocations) {
    for (j in 1:2) {
      txt = paste0(names(loca[[i]])[j], " = loca[[i]][[j]]")
      eval(parse(text = txt))
    }
  }
  
  field_args <- setNames(whenwhat_field$formula, whenwhat_field$name) %>%
    lapply(parse_expr)
  
  x <- list()
  j <- 1
  len <- length(unique(data$identifier))
  
  for (i in unique(data$identifier)) {
    datai = data %>%
      filter(identifier == i)
    x[[i]] <- datai %>%
      mutate(!!! field_args) %>%
      mutate_if(is.logical, as.integer)
    if (verbose) {
      cat("[", j, "/", len, "] Computed hbGIS for: ", i, "\n", sep = "")
      j <- j + 1
    }
  }
  data <- rbindlist(x) %>%
    st_set_geometry(data$geometry)
  return(data)
}
