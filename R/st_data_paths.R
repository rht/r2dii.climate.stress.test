#' Path to stress-test inputs
#'
#' This function helps expose hidden inputs to `run_stress_test()` so they are
#' clear and easier to program with.
#'
#' @param data Character. Path to the data directory.
#' @param project Character. Path to the project directory.
#'
#' @return A character vector.
#'
#' @examples
#' library(withr)
#'
#' st_data_paths()
#'
#' st_data_paths"/my/data", "/my/project/")
#'
#' local({
#'   local_envvar(st_data_paths"/my/data", "/my/project/"))
#'
#'   print(Sys.getenv("ST_DATA_PATH"))
#'   print(Sys.getenv("ST_PROJECT_FOLDER"))
#' })
#' Sys.getenv("ST_DATA_PATH")
#' Sys.getenv("ST_PROJECT_FOLDER")
#' @noRd
st_data_paths <- function(data = Sys.getenv("ST_DATA_PATH"),
                          project = Sys.getenv("ST_PROJECT_FOLDER")) {
  vctrs::vec_assert(data, character(), 1L)
  vctrs::vec_assert(project, character(), 1L)
  vctrs::vec_set_names(c(data, project), st_envvar_names())
}

#' Names of required environmental variables
#' @examples
#' st_envvar_names()
#' @noRd
st_envvar_names <- function() {
  c("ST_DATA_PATH", "ST_PROJECT_FOLDER")
}
