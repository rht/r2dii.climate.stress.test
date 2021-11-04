test_that("is sensitive to `quiet`", {
  skip("Temporarily dissable for speed")
  skip_if_not(is_me())

  expect_no_output(suppressWarnings(
    st(my_data(), asset_type = "bonds")
  ))
  expect_no_output(suppressWarnings(
    st(my_data(), asset_type = "bonds", quiet = TRUE)
  ))
  expect_output(suppressWarnings(
    st(my_data(), asset_type = "bonds", quiet = FALSE)
  ))
})

test_that("is sensitive to data", {
  skip("Temporarily dissable for speed")
  skip_if_not(is_me())

  expect_error(st(asset_type = "bonds"), "data.*missing")
  expect_no_error(suppressWarnings(
    st(my_data(), asset_type = "bonds")
  ))
})

test_that("outputs a data frame", {
  skip_if_not(is_me())

  data <- my_data()
  suppressWarnings(
    out <- st(data, asset_type = "bonds")
  )

  expect_s3_class(out, "data.frame")
  expect_true(hasName(out, "st_type"))
  expect_true(hasName(out, "st_name"))
})

