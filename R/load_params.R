#' load_params
#'
#' @param file Character to specify location of configuration file
#' @return list of parameters extract from the configuration file
#' @importFrom utils read.csv read.table
#' @export


load_params = function(file=c()) {
  expected_tsv_columns = c("value", "field", "subfield", "display", "class", "minimum",
                           "maximum",	"set", "description", "priority")
  
  params = read.csv(file = file, sep = ",")
  # remove duplicates, because sometimes GGIR config files have duplicates
  dups = duplicated(params)
  params = params[!dups,]
  # # Keep only parameters with a matching description in the description file
  # params_info_hbGIS_file = system.file("testfiles_hbGIS/params_description_hbGIS.tsv", package = "hbGIS")[1]
  # params_info_hbGIS = read.table(file = params_info_hbGIS_file, sep = "\t", header = TRUE)
  # params_info_hbGIS$id = with(params_info_hbGIS, paste0(field,  "__",  parameter))
  params$id = with(params, paste0(params$context, "__", params$name))
  # params_merged = merge(params_info_hbGIS, params, by.x = "id", by.y = "id")
  params_merged = params
  dups = duplicated(params_merged)
  params_merged = params_merged[!dups,]
  rownames(params_merged) = params_merged$id
  colnames(params_merged)[which(colnames(params_merged) == "formula")] = "value"
  # colnames(params_merged)[which(colnames(params_merged) == "id")] = "field"
  colnames(params_merged)[which(colnames(params_merged) == "name")] = "subfield"
  expected_tsv_columns = c(expected_tsv_columns, "is_where_field", "after_conversion")
  params = params_merged[, expected_tsv_columns]
  params = params[,-which(colnames(params) %in% c("subfield", "id", "field"))]
  
  return(params)
}