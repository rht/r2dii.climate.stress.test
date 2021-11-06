#' A version of `run_stress_test()` with visible inputs and outputs
#'
#' @param data Character vector of length 2. Paths to the directories
#'   ST_INPUTS_MASTER and ST_TESTING_\{aset-type\}, e.g. ST_TESTING_BONDS,
#'   respectively. These directories must have the files expected by the
#'   stress-test project.
#' @param asset_type An atomic character vector. Either "bonds", "equity", or
#'   "loans".
#'
#' @return A data frame.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' out <- st(my_data(), asset_type = "bonds")
#'
#' out %>%
#'   filter(st_type == "bonds", st_name == "comp") %>%
#'   unnest(cols = result)
#'
#' out %>%
#'   filter(st_type == "bonds", st_name == "comp") %>%
#'   unnest(result)
#' @noRd
st <- function(data, asset_type, ..., quiet = TRUE) {
  vec_assert(data, character(), size = 2L)
  data <- vec_set_names(data, envvar_names())
  local_envvar(data)

  control_verbosity <- ifelse(quiet, utils::capture.output, identity)
  invisible(control_verbosity(run_stress_test(asset_type = asset_type, ...)))

  outputs_path() %>%
    dir_ls() %>%
    map(~ read_csv(.x, show_col_types = FALSE)) %>%
    enframe(value = "st_result") %>%
    restructure_st()
}

restructure_st <- function(data) {
  data %>%
    mutate(name = path_file(name)) %>%
    mutate(name = path_ext_remove(name)) %>%
    mutate(name = sub("^stress_test_results_", "", name)) %>%
    extract(name, into = c("st_type", "st_name"), "([^_]+)_(.*)")
}

envvar_names <- function() {
  c("ST_DATA_PATH", "ST_PROJECT_FOLDER")
}
