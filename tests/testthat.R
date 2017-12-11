library(testthat)
library(heartrate)


data("tester")
test_that("target_range works", {
  expect_equal(unname(target_ranges(tester$`Resting Heart Rate`, time_data = tester$Day)[1,2]), 77)
})
