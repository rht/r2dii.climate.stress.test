test_that("is sensitive to `quiet`", {
  #skip("Temporarily dissable for speed")
  skip_if_not(is_registered_dev())

  expect_no_output(suppressWarnings(
    st(st_data_paths(), asset_type = "bonds")
  ))
  expect_no_output(suppressWarnings(
    st(st_data_paths(), asset_type = "bonds", quiet = TRUE)
  ))
  expect_output(suppressWarnings(
    st(st_data_paths(), asset_type = "bonds", quiet = FALSE)
  ))
})

test_that("is sensitive to data", {
  #skip("Temporarily dissable for speed")
  skip_if_not(is_registered_dev())

  expect_error(st(asset_type = "bonds"), "data.*missing")
  expect_no_error(suppressWarnings(
    st(st_data_paths(), asset_type = "bonds")
  ))
})

test_that("outputs a data frame", {
  # #skip("Temporarily dissable for speed")
  skip_if_not(is_registered_dev())

  suppressWarnings(
    out <- st(st_data_paths(), asset_type = "bonds")
  )

  expect_s3_class(out, "data.frame")
  expect_true(hasName(out, "st_type"))
  expect_true(hasName(out, "st_name"))
})

test_that("with bad `data` errors gracefully", {
  bad <- c(1:2, NULL)
  expect_error(
    class = "vctrs_error_assert_ptype",
    st(bad, asset_type = "bonds")
  )

  bad <- c("wrong/size", NULL)
  expect_error(st(bad, asset_type = "bonds"), class = "vctrs_error_assert_size")

  bad <- c("invalid", "path")
  expect_error(st(bad, asset_type = "bonds"), "valid.*not")
})

