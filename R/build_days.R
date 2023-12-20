
#' Calculate day-level summaries from the whenwhat dataset
#'
#' @description Build a days dataset by summarising \code{whenwhat}
#' by day and person (\code{identifier}). 
#'
#' @param data The whenwhat data obtained from \code{\link{build_whenwhatwhere}}.
#' @param verbose Print progress to console. Default is \code{TRUE}.
#' @param where_field ...
#' @param whenwhat_field ...
#' @param loca Nested list with location information
#' @param participant_basis participant_basis
#'#'
#' @return A table summarised by day.
#'
#' @import dplyr
#' @import sf
#' @importFrom rlang parse_expr
#' @importFrom purrr reduce
#'
#' @export
# Code modified from https://thets.github.io/palmsplusr/
build_days <- function(data = NULL, verbose = TRUE, 
                       where_field = NULL,
                       whenwhat_field = NULL,
                       loca = NULL,
                       participant_basis = NULL) {
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
  
  duration = datetime = name = is_where_field = NULL
  where_field <- where_field %>% filter(is_where_field == TRUE)
  where_names <- where_field %>% pull(name)
  
  if (is.null(where_names)) {
    where_names <- "total"
  } else {
    where_names <- c("total", where_names)
  }
  where_args <- setNames("1", "total") %>% lapply(parse_expr)
  where_args <- c(where_args, setNames(where_field[[2]], where_field[[1]]) %>%
                     lapply(parse_expr))
  data <- data %>%
    mutate(!!! where_args) %>%
    mutate_if(is.logical, as.integer)
  
  fields <- whenwhat_field %>% filter(is_where_field == TRUE) %>% pull(name)

  data <- data %>%
    st_set_geometry(NULL) %>%
    dplyr::select(identifier, datetime, any_of(where_names), all_of(fields)) %>%
    mutate(duration = 1) %>%
    mutate_at(vars(-identifier,-datetime), ~ . * palms_epoch(data) / 60) %>%
    group_by(identifier, date = as.Date(datetime)) %>%
    dplyr::select(-datetime)
  
  x <- list()
  for (i in where_names) {
    x[[i]] <- data %>%
      filter(!!(as.name(i)) > 0) %>%
      dplyr::select(-any_of(where_names), duration) %>%
      summarise_all(~ sum(.)) %>%
      ungroup() %>%
      rename_at(vars(-identifier, -date), ~ paste0(i, "_", .))
  }
  
  result <- x %>%
    reduce(left_join, by = c("identifier" = "identifier", "date" = "date"))
  
  # Count the number of segments per where per day per identifier
  segmentcount = function(x) {
    x = as.numeric(unlist(x)) # x is a tibble column, so first convert to numeric vector
    return(length(which(rle(x)$values != 0)))
  }
  
  for (dom in where_names) {
    if (dom != "total") {
      result[, dom] <- NA
      for (id in unique(result$identifier)) {
        for (date in as.Date(unique(result$date[which(result$identifier == id)]))) {
          CNT = segmentcount(data[which(data$identifier == id & data$date == date), dom])
          result[which(result$identifier == id & result$date == date), dom] = CNT
        }
      }
      names(result)[which(names(result) == dom)] = paste0(dom, "_segmentcount")
    }
  }
  return(result)
}
