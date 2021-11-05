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
  UseMethod("st")
}

st.default <- function(data, asset_type, ..., quiet = TRUE) {
  msg <- glue("Can't deal with objects of class {class(data)}")
  abort(msg, class = "unsupported_class")
}

st.character <- function(data, asset_type, ..., quiet = TRUE) {
  vec_assert(data, size = 2L)
  st_impl(data = data, asset_type = asset_type, ..., quiet = quiet)
}

st_impl <- function(data, asset_type, ..., quiet = TRUE) {
  local_envvar(data)

  control_verbosity <- ifelse(quiet, utils::capture.output, identity)
  invisible(control_verbosity(run_stress_test(asset_type = asset_type, ...)))

  outputs_path() %>%
    dir_ls() %>%
    map(~read_csv(.x, show_col_types = FALSE)) %>%
    enframe() %>%
    clean_name()
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
