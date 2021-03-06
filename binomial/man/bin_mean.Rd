% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_measures.R
\name{bin_mean}
\alias{bin_mean}
\title{Binomial Mean}
\usage{
bin_mean(trials, prob)
}
\arguments{
\item{trials}{the number of trials}

\item{prob}{number that is the probability of success}
}
\value{
mean the numerical mean
}
\description{
computes mean given trials and probs
}
\examples{
#5 trials with success prob at 0.5
bin_mean(trials=5, prob=0.5)
}
