st_map <- function(data, asset_type, ..., quiet = TRUE) {
  dots <- list2(...)
  long <- keep(dots, ~length(.x) > 1L)

  abort_if_no_argument_is_long(long)
  abort_if_more_than_one_argument_is_long(long)

  dots1 <- dots[setdiff(names(dots), names(long))]
  args1 <- list2(
    data = data,
    asset_type = asset_type,
    !!!dots1,
    quiet = quiet
  )

  nms <- names(long)
  val <- unlist(long)
  x <- vec_set_names(val, glue("{nms}___{val}"))
  args_lst <- map(x, ~append(args1, vec_set_names(.x, nms)))

  args_lst %>%
    map(~exec(st, !!!.x)) %>%
    enframe(value = "st_result") %>%
    restructure_st_map()
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

restructure_st_map <- function(data) {
  data %>%
    separate(.data$name, into = c("arg_name", "arg_value"), sep = "___") %>%
    unnest(cols = .data$st_result) %>%
    relocate(.data$st_type, .data$st_name)
}
