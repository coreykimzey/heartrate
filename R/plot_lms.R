
plot_lms <- function(dataset, col_names, response_variable, multivariate = FALSE, ...){
  if(multivariate == FALSE){
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
  }else{
    response_col <- dataset[[response_variable]]
    predictor_col <- matrix(nrow = nrow(dataset), ncol = length(col_names))
    summary_line = NULL
    par(mfrow=c(ceiling(length(col_names)/2), 2))
    for(i in 1:length(col_names)){
    predictor_col[,i] <- dataset[[col_names[i]]]
    }

    line_needed <- lm(response_col ~ predictor_col)
    names(line_needed$coefficients)[2:(length(col_names)+1)] <- col_names
    for(j in 1:length(col_names)){
      summary_line <- summary(line_needed)
      plot(predictor_col[,j],
           response_col,
           ylab = response_variable,
           xlab = col_names[j],
           main = paste("Resting =", round(line_needed$coefficients[j+1], 2),
                        col_names[j], "+", round(line_needed$coefficients[1],2)),
           sub = paste("Coefficient P-value = ", round(summary_line$coefficients[j+1,4],3)))
      abline(a = line_needed$coefficients[1], b = line_needed$coefficients[(j+1)] )


    }
    }


}
