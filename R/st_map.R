st_map <- function(data, asset_type, ..., quiet = TRUE) {
  dots <- rlang::list2(...)
  long <- purrr::keep(dots, ~length(.x) > 1L)

  if (identical(length(long), 0L)) {
    abort(c(
      "Must privide one argument of `run_stress_test()` with multiple values.",
      i = "Do you need to use `run_stress_test()` instead?"
    ))
  }

  if (length(long) > 1L) {
    abort(c(
      "Must provide no more than one argument with multiple values.",
      x = glue("Arguments with multiple values: {toString(names(long))}."),
      i = "Did you forget to pick only one?"
    ))
  }

  dots1 <- dots[setdiff(names(dots), names(long))]
  args1 <- rlang::list2(
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

restructure_st_map <- function(data) {
  data %>%
    separate(.data$name, into = c("arg_name", "arg_value"), sep = "___") %>%
    unnest(cols = .data$st_result) %>%
    relocate(.data$st_type, .data$st_name)
}
