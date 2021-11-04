#' A version of `run_stress_test()` with visible inputs and outputs
#'
#' Inspired by https://design.tidyverse.org/
#'
#' @param data A named character vector of length 2. The names must be
#'   "ST_DATA_PATH" and "ST_PROJECT_FOLDER", and the values must be the paths to
#'   directories structured as expected by the stress-test project.
#' @param asset_type An atomic character vector. Either "bonds", "equity", or
#'   "loans".
#'
#' @return A data frame.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' data <- grep("ST_", Sys.getenv(), value = TRUE)
#'
#' out <- st(data, asset_type = "bonds")
#'
#' out %>%
#'   filter(st_type == "bonds", st_name == "comp") %>%
#'   unnest(cols = value)
#'
#' out %>%
#'   filter(st_type == "bonds", st_name == "comp") %>%
#'   unnest(value)
#' @noRd
st <- function(data, asset_type, ..., quiet = TRUE) {
  local_envvar(data)

  f <- ifelse(quiet, utils::capture.output, identity)
  invisible(f(run_stress_test(asset_type = asset_type, ...)))

  paths <- dir_ls(outputs_path())
  out <- map(paths, ~read_csv(.x, show_col_types = FALSE))
  out <- enframe(out)
  out <- clean_name(out)
}

#' Clean the output of `st()`
#' @noRd
clean_name <- function(data) {
  data %>%
    mutate(name = path_file(name)) %>%
    mutate(name = path_ext_remove(name)) %>%
    mutate(name = sub("^stress_test_results_", "", name)) %>%
    extract(name, into = c("st_type", "st_name"), "([^_]+)_(.*)")
}
