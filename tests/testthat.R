library(testthat)
library(heartrate)


data("ckheartRate")
test_that("target_range works", {
  expect_equal(unname(target_ranges(ckheartRate$`Resting Heart Rate`, time_data = ckheartRate$Day)[1,2]), 77)
})
