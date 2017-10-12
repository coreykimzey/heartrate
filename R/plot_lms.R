
plot_lms <- function(dataset, col_names, response_variable, ...){
  summary_line = NULL
  par(mfrow=c(ceiling(length(col_names)/2), 2))
  for(i in 1:length(col_names)){

    response_col <- dataset[[response_variable]]
    predictor_col <- dataset[[col_names[i]]]
    line_needed <- lm(response_col ~ predictor_col)
    summary_line <- summary(line_needed)
    plot(predictor_col,
         response_col,
         ylab = response_variable,
         xlab = col_names[i],
         main = paste("Resting =", round(line_needed$coefficients[2], 2),
                      col_names[i], "+", round(line_needed$coefficients[1],2)),
        sub = paste("Coefficient P-value = ", round(summary_line$coefficients[2,4],3)))
    abline(a = line_needed$coefficients[1], b = line_needed$coefficients[2], ... )

  }


}
