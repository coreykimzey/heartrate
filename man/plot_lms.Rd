\name{plot_lms}
\alias{plot_lms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots multiple scatterplots with linear regression line
}
\description{
This function will take multiple column names and plot scatterplots against "Resting Heart Rate". Then it fits a linear regression and adds that line. It also says at the bottom what the p-value for the slope coefficient is.
}
\usage{
plot_lms(dataset, col_names, response_variable, multivariate = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
This should just be a dataset with a column name "Resting Heart Rate"
}
  \item{col_names}{
This specifies with columns to plot and do the linear regression.
}
  \item{response_variable}{
A character string of the column name of the response variable
}
  \item{multivariate}{
This is should either be true or false. This determines if plot_lms fits a multivariate regression or it fits a linear regression to each of the columns selected.
}
  \item{\dots}{
This will pass to the abline function. Therefore, it will control things like color of that line. It doesn't currently do anything to the plotting of the points.
}
}

\value{
It will return as many plots as the length of the col_names vector. It shows the regression line fit (whether single or multivariate regression). It also puts the function at the top of the figure.
}

\author{
Corey Kimzey
}


\seealso{
 \code{\link{plot_cor}},
}
\examples{
data(tester)
plot_lms(tester, col_names  = c("Inf", "Computing"), response_variable = "Resting Heart Rate")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
