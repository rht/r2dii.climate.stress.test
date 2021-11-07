#' Sensitivity analysis: `run_stress_test()` multiple times
#'
#' These functions performs the so called "sensitivity analysis" for the asset
#' type in their name. They run [run_stress_test()] multiple times, iterating
#' over multiple values of one and only one argument of [run_stress_test()].
#'
#' @param data Character vector of length 2. Paths to the directories
#'   ST_INPUTS_MASTER and ST_TESTING_\{aset-type\}, e.g. ST_TESTING_BONDS,
#'   respectively. These directories must have the files expected by the
#'   stress-test project. The easiest way to create this object is with
#'   [st_data_paths()].
#' @param ... Arguments passed to [run_stress_test()].
#' @param quiet Logical. Print non-condiiton messages to the console?
#'
#' @seealso [st_data_paths()].
#' @family main functions
#'
#' @return A data frame.
#'
#' @export
#'
#' @examplesIf r2dii.climate.stress.test:::is_registered_dev()
#' data <- st_data_paths(
#'   data = "/home/mauro/tmp/st/ST_INPUTS_MASTER",
#'   project = "/home/mauro/tmp/st/ST_TESTING_BONDS"
#' )
#'
#' out <- st_bonds(data, term = c(1, 2), shock_year = 2031)
#'
#' # The data frame output helps you quickly explore and manipulate your results
#' subset(out, st_name == "port" & arg_value == 2)
st_bonds <- function(data, ..., quiet = TRUE) {
  rerun_st(data, asset_type = "bonds", ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_equity <- function(data, ..., quiet = TRUE) {
  rerun_st(data, asset_type = "equity", ..., quiet = quiet)
}

#' @rdname st_bonds
#' @export
st_loans <- function(data, ..., quiet = TRUE) {
  rerun_st(data, asset_type = "loans", ..., quiet = quiet)
}

rerun_st <- function(data, asset_type, ..., quiet = TRUE) {
  args <- enlist_args(data, asset_type, ..., quiet = quiet)

  args %>%
    map(~ exec(st, !!!.x)) %>%
    enframe(value = "st_result") %>%
    restructure_rerun_st() %>%
    unnest(st_result)
}

enlist_args <- function(data, asset_type, ..., quiet) {
  dots <- list2(...)
  long <- keep(dots, ~ length(.x) > 1L) %>%
    abort_if_no_argument_is_long() %>%
    abort_if_more_than_one_argument_is_long()

  dots1 <- dots[setdiff(names(dots), names(long))]
  args1 <- list2(data = data, asset_type = asset_type, !!!dots1, quiet = quiet)
  nms <- names(long)
  val <- unlist(long)

  vec_set_names(val, glue("{nms}___{val}")) %>%
    map(~ append(args1, vec_set_names(.x, nms)))
}

abort_if_no_argument_is_long <- function(data) {
  if (identical(length(data), 0L)) {
    abort(c(
      "Must privide one argument of `run_stress_test()` with multiple values.",
      i = "Do you need to use `run_stress_test()` instead?"
    ))
  }

  invisible(data)
}

abort_if_more_than_one_argument_is_long <- function(data) {
  if (length(data) > 1L) {
    abort(c(
      "Must provide no more than one argument with multiple values.",
      x = glue("Arguments with multiple values: {toString(names(data))}."),
      i = "Did you forget to pick only one?"
    ))
  }

  invisible(data)
}

restructure_rerun_st <- function(data) {
  data %>%
    separate(.data$name, into = c("arg_name", "arg_value"), sep = "___") %>%
    unnest(cols = .data$st_result) %>%
    relocate(.data$st_type, .data$st_name)
}

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
