
plot_cor <- function(dataset, col_names, response_variable ){
  par(mfrow=c(ceiling(length(col_names)/2), 2))
  for(i in 1:length(col_names)){
    plot(dataset[[response_variable]],
         dataset[[col_names[i]]],
         xlab = "Resting Heart Rate",
         ylab = col_names[i],
         main = paste("Correlation:", round(cor(dataset[,response_variable],
                                          dataset[,col_names[i]], use =  "complete.obs"),
                                          digits = 3)))

  }


}
