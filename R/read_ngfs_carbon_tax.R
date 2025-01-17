#' Read in NGFS carbon tax data from csv and check that all expected columns
#' are given.
#'
#' @param path A string that points to the location of the file containing the
#'   NGFS carbon tax information
#'
#' @family import functions
read_ngfs_carbon_tax <- function(path = NULL) {
  path %||% stop("Must provide 'path'")

  data <- validate_file_exists(path) %>%
    readr::read_csv(
    col_types = readr::cols(
      year = "d",
      model = "c",
      scenario = "c",
      scenario_geography = "c",
      variable = "c",
      unit = "c",
      carbon_tax = "d"
    )
  )

  expected_columns <- c(
    "year", "model", "scenario", "scenario_geography", "variable", "unit",
    "carbon_tax"
  )

  validate_data_has_expected_cols(
    data = data,
    expected_columns = expected_columns
  )

  return(data)
}
