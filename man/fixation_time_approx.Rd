% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixationTime.R
\name{fixation_time_approx}
\alias{fixation_time_approx}
\title{Fixation Time in Wright-Fisher Model with selection by Taylor expansion}
\usage{
fixation_time_approx(N, alpha, p, k = 100, type = "2")
}
\arguments{
\item{N}{Number of alleles in the considered population (population size)}

\item{alpha}{Selection coefficient}

\item{p}{Vector or initial probabilities (values between 0 and 1)}

\item{k}{number of elements in the Taylor expansion}

\item{type}{Type of approximation to use (1 or 2)}
}
\value{
the time to fixation using an approximation by Taylor expansion
}
\description{
This function computes the time to fixation for a given initial probability and fixed selection coefficient alpha using a Taylor expansion
}
