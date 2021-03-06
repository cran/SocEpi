% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_score_fun.R
\name{zscore}
\alias{zscore}
\title{z-scores}
\usage{
zscore(population, variable)
}
\arguments{
\item{population}{Population counts.}

\item{variable}{Continuous deprivation measure, such as a percentage, to calculate the z-scores for.}
}
\value{
A list including the following:
 \item{z.score}{standardized score using weighted mean and sd}
 \item{w.mean}{weighted mean}
 \item{w.sd}{weighted standard deviation}
 \item{weight}{population weight}
}
\description{
\code{zscores} calculates standardized scores or z-scores, with a mean close to zero and standard deviation (sd)
close to one, for the variable given in \code{variable}.
The standardization uses a population weighted mean and standard deviation, which are calculated based on the
population distribution given by \code{population}.
}
\examples{
data <- dep_data

# store all results in object z_oc
z_oc <- zscore(data$total_pop, data$pcnt_overcrowding)

# extract z-score
data$z_overcrowd <- z_oc$z.score
mean(data$z_overcrowd) # mean of z-score
sd(data$z_overcrowd) # sd z-score

# extract weighted mean and sd, compare weighted values to unweighted values
mean(data$pcnt_overcrowding)
z_oc$w.mean
sd(data$pcnt_overcrowding)
z_oc$w.sd

}
