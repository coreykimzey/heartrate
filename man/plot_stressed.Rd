\name{plot_stressed}
\alias{plot_stressed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing Actual Data to Moving Average
}
\description{
This is a function used to compare actual data to the moving average on a plot.
}
\usage{
plot_stressed(dataset_heartRate, dataset_time = NULL, order_of_smoother = 3,
      na.remove = TRUE, col_ma = "Red", col_dataset = "black", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset_heartRate}{
This should be a vector of the resting heart rate.
}
  \item{dataset_time}{
This should be a vector of the the time periods.
}
  \item{order_of_smoother}{
This will be the number of time periods to average for the moving average.
}
  \item{na.remove}{
Currently, this part of the function doesn't work. The function will always remove any na's in the dataset_time or dataset_heartRate.
}
  \item{col_ma}{
This is for the color of the moving average.
}
  \item{col_dataset}{
This defines the color for the actual data.
}
  \item{\dots}{
This provides the ability to change some of the properties of the plot.
}
}
\details{
This function currently plots the the moving average and the actual data. I think I should add some better labels.
}
\value{
It will return a plot.
}

\author{
Corey Kimzey
}



\examples{
data(tester)
plot_stressed(tester$"Resting Heart Rate")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stressed }% use one of  RShowDoc("KEYWORDS")

