test_that("is sensitive to `asset_type`", {
  out <- st_map(st_data_paths(), asset_type = "bonds")
  expect_equal(out$asset_type, "bonds")
})

test_that("defaults to run quietly", {
  out <- st_map(st_data_paths(), asset_type = "bonds")
  expect_equal(out$quiet, TRUE)
})

test_that("with more than one long stress-test argument errors gracefully", {
  long1 <- range(lgd_senior_claims_range_lookup)
  expect_no_error(
    st_map(
      st_data_paths(),
      "bonds",
      lgd_senior_claims = long1,
      term = 1
    )
  )

  long2 <- range(terminal_value_range_lookup)
  expect_snapshot_error(
    st_map(
      st_data_paths(),
      "bonds",
      lgd_senior_claims = long1,
      terminal_value = long2,
      term = 1
    )
  )
})

test_that("iterates over the long argument", {
  long1 <- range(lgd_senior_claims_range_lookup)
  out <- st_map(
    st_data_paths(),
    "bonds",
    lgd_senior_claims = long1,
    term = 1
  )
  expect_s3_class(out, "data.frame")
  expect_named(out, c("st_type", "st_name", "arg_name", "arg_value", "result"))
})
