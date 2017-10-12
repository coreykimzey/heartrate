

target_ranges <- function(heartRate_data, time_data, age = 25, low_range_percent = 50, high_range_percent = 75) {
  #I tried to use tidyverse in this part but it forced everything into a timeseries.
  # So I went back to base R
  date_given <- time_data[ !is.na(heartRate_data)]
  heartRate_data <- heartRate_data [!is.na(heartRate_data)]
  max_heart_rate <-  220 - age
  heart_rate_reserve <- max_heart_rate - heartRate_data
  low_range <- low_range_percent/100 * heart_rate_reserve
  high_range <- high_range_percent/100 * heart_rate_reserve
  return(cbind.data.frame(date_given, low_range, high_range))
}
