test_that("with missing `asset_type` errors gracefully", {
  expect_snapshot_error(run_stress_test())
})

test_that("with multiple values of some argumeht errors gracefully", {
  too_long <- 1:2
  expect_snapshot_error(run_stress_test("bonds", too_long))
})

test_that("as a side-effect function, ir returns first argument invisibly", {
  expect_invisible(out <- purrr::quietly(run_stress_test)("bonds"))
  expect_equal(out$result, "bonds")
})

test_that("populates the output directory", {
  skip_if_not(is_mauro())

  dir_empty(outputs_path())
  purrr::quietly(run_stress_test)("bonds")

  expect_false(is_empty_dir(outputs_path()))
  expect_length(ls_results(outputs_path()), 5L)
})
