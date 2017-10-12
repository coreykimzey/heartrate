\name{plot_stressed}
\alias{plot_stressed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing Actual Data to Moving Average
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Corey Kimzey
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ckheartRate)
plot_stressed(ckheartRate$"Resting Heart Rate")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stressed }% use one of  RShowDoc("KEYWORDS")

