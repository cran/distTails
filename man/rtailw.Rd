% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weibull-random.R
\name{rtailw}
\alias{rtailw}
\title{TailW Random Sample Generation}
\usage{
rtailw(n, threshold, scale, shape)
}
\arguments{
\item{n}{Sample size.}

\item{threshold}{Minimum value of the tail.}

\item{scale}{Scale parameter.}

\item{shape}{Shape parameter.}
}
\value{
Gives random deviates of the TailW. The length of the result is determined by n.
}
\description{
This function generates random deviates for the tailW distribution.
}
\examples{
x <- rtailw(1000, 1, 2, 3)
hist(x, breaks = "FD")
}
\keyword{TailW}
