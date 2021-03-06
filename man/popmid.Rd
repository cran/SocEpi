% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/popmid_fun.R
\name{popmid}
\alias{popmid}
\title{Population midpoints}
\usage{
popmid(x)
}
\arguments{
\item{x}{Vector of population distribution that sums to 1.}
}
\value{
A vector of population midpoints.
}
\description{
\code{popmid} calculates the cumulative midpoints for population percentiles that can then be used as the
  predictor in the OLS model for calculating the slope, i.e. the slope index of inequality (SII).
  For internal use within the \code{rii} function.
}
\keyword{internal}
