\name{plot_cor}
\alias{plot_cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Plotting with Correlation as Title
}
\description{
This function plots resting heart rate versus the mentioned columns. It then posts the corelation between those two variable as the title.
}
\usage{
plot_cor(dataset, col_names, response_variable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
This should be a dataset with "Resting Heart Rate" as one of the columns.
}
  \item{col_names}{
A vector of the column names that you are interested in looking at their scatterplot compared to "Resting Heart Rate"
}
  \item{response_variable}{
A character string of the column name of the response variable
}
}

\value{
It will return a series of plots, with the correlation as the title between the variables.
}

\author{
Corey Kimzey
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot_lms}}
}
\examples{
data(tester)
plot_cor(tester, col_names = c("Inf", "Computing"), response_variable = "Resting Heart Rate")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation}% use one of  RShowDoc("KEYWORDS")

