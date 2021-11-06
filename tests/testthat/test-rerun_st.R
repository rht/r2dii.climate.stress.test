test_that("with no argument with multiple values errors gracefully", {
  skip_if_not(is_registered_dev())

  expect_snapshot_error(rerun_st(st_data_paths(), asset_type = "bonds", term = 1))
})

test_that("with more than one long stress-test argument errors gracefully", {
  skip_if_not(is_registered_dev())

  long1 <- range(lgd_senior_claims_range_lookup)
  expect_no_error(
    suppressWarnings(
      rerun_st(
        st_data_paths(),
        "bonds",
        lgd_senior_claims = long1,
        term = 1
      )
    )
  )

  long2 <- range(terminal_value_range_lookup)
  expect_snapshot_error(
    rerun_st(
      st_data_paths(),
      "bonds",
      lgd_senior_claims = long1,
      terminal_value = long2,
      term = 1
    )
  )
})

test_that("iterates over the long argument", {
  skip_if_not(is_registered_dev())

  long1 <- range(lgd_senior_claims_range_lookup)
  out <- suppressWarnings(
    rerun_st(
      st_data_paths(),
      "bonds",
      lgd_senior_claims = long1,
      term = 1
    )
  )

  expect_s3_class(out, "data.frame")
  nms <- c("st_type", "st_name", "arg_name", "arg_value", "st_result")
  expect_named(out, nms)
})

test_that("works with integer inputs of `term`", {
  data <- st_data_paths()
  expect_no_error(
    rerun_st(data, "bonds", term = 1:2)
  )
})
