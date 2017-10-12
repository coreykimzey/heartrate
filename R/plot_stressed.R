#stressed

plot_stressed <- function(dataset_heartRate, dataset_time = NULL, order_of_smoother = 3,
                          na.remove = TRUE, col_ma = "Red", col_dataset = "black", ...){
  #again I had switched things into tidyverse but then it switched everything to time series

  dataset_time <- dataset_time[!is.na(dataset_heartRate)]
  dataset_heartRate <- dataset_heartRate[!is.na(dataset_heartRate)]


  moving_average <- ma(dataset_heartRate, order_of_smoother)

  if(is.null(dataset_time)){

    plot(moving_average, col = col_ma,
          ylim = c(min(dataset_heartRate, moving_average, na.rm = TRUE),
                       max(dataset_heartRate,
                       moving_average, na.rm = TRUE)))
    lines(dataset_heartRate, col = col_dataset)
  }else {
    plot(dataset_time, moving_average, type = "l", col = col_ma, ...,
         ylim = c(min(dataset_heartRate, moving_average, na.rm = TRUE),
                  max(dataset_heartRate,
                  moving_average, na.rm = TRUE)))
    lines(dataset_time,dataset_heartRate, col = col_dataset)
    }
  legend( "bottomright",legend = c("Moving Ave", "Actual"), lwd = 1, col = c(col_ma, col_dataset))
}
