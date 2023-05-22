#' Fourier transforms from pairwise distance matrix
#'
#' `get_weight_measure` returns either `mu_hat_gaussian` or `mu_hat_szekely`.
#'
#' @param x Matrix of pairwise distances.
#' @param index Numeric between 0 and 2.
#' @rdname fourier_transforms
mu_hat_szekely <- function(x, index = 1) {
    if (index == 1) return(x)
    x^index
}
#'
#' @rdname fourier_transforms
#' @param sigma2 Variance of gaussian measure.
mu_hat_gaussian <- function(x, sigma2 = 0.25) {
    exp(-x^2 * sigma2 / 2)
}

#' @rdname fourier_transforms
#' @param mu Single character string of weight measure to be used.
get_weight_measure <- function(
    mu = c("szekely", "gaussian"), index = 1, sigma2 = 0.25
) {
    switch(mu,
        "szekely" = function(x) mu_hat_szekely(x, index),
        "gaussian" = function(x) mu_hat_gaussian(x, sigma2)
    )
}

#' Fast pairwise Euclidean distances
#'
#' Calculates pairwise Euclidean distances of univariate time series.
#'
#' @param x A univariate time series.
#' @param mu_hat Fourier transform function wrt some weight measure.
#' @useDynLib adcf
#' @returns A `length(x) * length(x)` matrix of pairwise Euclidean distances.
pairwise_dist <- function(x, mu_hat) {
    ret <- vapply(x, function(xdot) abs(x - xdot), double(vctrs::vec_size(x)))
    mu_hat(ret)
}