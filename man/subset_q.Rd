% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_q_fun.R
\name{subset_q}
\alias{subset_q}
\title{Subset data}
\usage{
subset_q(df, condition, variables)
}
\arguments{
\item{df}{Data frame to be subsetted.}

\item{condition}{Subsetting condition, such as which rows to select.}

\item{variables}{Variables to include in subset.}
}
\value{
A smaller data frame of subsetted data.
}
\description{
Internal use only, allows subsetting within another function (for use of NSE in functions).
}
\keyword{internal}
