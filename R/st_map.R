st_map <- function(data, asset_type, ..., quiet = TRUE) {
  dots <- rlang::list2(...)
  long <- purrr::keep(dots, ~length(.x) > 1L)
  if (length(long) > 1L) {
    abort(c(
      "Must provide no more than one argument with multiple values.",
      x = glue("Arguments with multiple values: {toString(names(long))}."),
      i = "Did you forget to pick only one?"
    ))
  }

  rlang::list2(
    asset_type = asset_type,
    !!!dots,
    quiet = quiet
  )
}
