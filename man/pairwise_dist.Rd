% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcf_helpers.R
\name{pairwise_dist}
\alias{pairwise_dist}
\title{Fast pairwise Euclidean distances}
\usage{
pairwise_dist(x, mu_hat)
}
\arguments{
\item{x}{A univariate time series.}

\item{mu_hat}{Fourier transform function wrt some weight measure.}
}
\value{
A \code{length(x) * length(x)} matrix of pairwise Euclidean distances.
}
\description{
Calculates pairwise Euclidean distances of univariate time series.
}
