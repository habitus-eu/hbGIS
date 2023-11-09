#' update_params
#'
#' @param file Character to specify location of original configuration file
#' @param new_params New parameters
#' @return No object returned, function only reads original data, and overwrites parameters and stores it again
#' @importFrom utils read.csv write.csv
#' @export

update_params = function(new_params = c(), file = c()) {
  
  overwriteMatchingFields = function(params, new_params, format = "") {
    # Remove duplicates
    dups = duplicated(params$argument)
    params = params[!dups,]
    rownames(params) = params$argument
    # Only overwrite the matching fields of csv file
    for (j in 1:nrow(new_params)) {
      ind = which(rownames(params) %in% rownames(new_params)[j] == TRUE)
      if (length(ind) > 0) {
        if (format == "csv_hbGIS") {
          if (new_params$value[j] != params$formula[ind]) {
            params$formula[ind] = new_params$value[j]
          }
        } else {
          params$value[ind] = new_params$value[j]
        }
      }
    }
    return(params)
  }
  params = read.csv(file = file, sep = ",")
  params$argument = with(params, paste0(params$context, "__",params$name))
  params = overwriteMatchingFields(params, new_params, format)
  params = params[,-which(colnames(params) %in% c("argument"))]
  write.csv(x = params, file = file, row.names = FALSE)
  
}