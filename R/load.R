#' Load Raw Astro Data
#'
#' @param data_dir Directory where the data files are located.
#'
#' @return Data frame containing the raw chemical abundance data and metadata.
load_astro_data <- function(data_dir = here::here("data")) {
  data <- readr::read_csv(file.path(data_dir, "allstars_gc.csv"))
  return(data)
}
