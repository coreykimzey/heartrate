\name{target_ranges}
\alias{target_ranges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Burning Fat Ranges based on Age and Resting Heart Rate
}
\description{
This function computes best fat burning range based off your age and resting heart rate according to active.com.
}
\usage{
target_ranges(heartRate_data, time_data, age = 25, low_range_percent = 50,
high_range_percent = 75)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{heartRate_data}{
This should be a vector containing the resting heart rates. Any NA's will be removed automatically from the data set.
}
  \item{time_data}{
This should be a vector containing the dates (or other time values.) Any NA's will be removed automatically from the data set.
}
  \item{age}{
This is your age in years.
}
  \item{low_range_percent}{
  This is the percent that will the low end of the target range. Active.com suggests 50 percent. This number should be the percent ie low_range_percent = 50 is 50 percent.
  }
  \item{high_range_percent}{
  This is the percent that will the high end of the target range. Active.com suggests 75 percent. This number should be the percent ie high_range_percent = 75 is 75 percent.
  }
}
\details{
This calculation should give you your target range for your heart rate for excersise.
}
\value{
This function returns a matrix of the low range and the high range for your target heart rate.
}
\references{
\url{https://www.active.com/fitness/articles/how-to-calculate-your-training-heart-rate-zones}
}
\author{
Corey Kimzey 1st Year PHD Student Univesity of Rochester Department of Computational Biology and Biostatistics
}

\examples{
#This is based off of the sample data
data(tester)
target_ranges(tester$`Resting Heart Rate`, tester$Day)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
