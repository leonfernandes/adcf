#' Auto distance covariance
#'
#' Calculates the auto-distance covariance/correlation of `x`.
#'
#' @param x numeric vector.
#' @param lags integer. Specifies lags at which distance covariance is to be
#'      calculated. When `NULL` maximum range of lags are used.
#' @param mu Single character string corresponding to the weight measure to be
#'      used. Must be one of "szekely" or "gaussian".
#' @param ... Optional parameters supplied for the weight measure `mu`.
#' @export
#' @returns A `tibble` with columns `lag`, `auto_dist_covariance` and
#'      `auto_dist_correlation`.
#' @examples
#' # adcf ----------------------------------------------------------------------
#' x <- rnorm(100)
#' # Adcf values are small for all lags due to independence
#' adcf(x)
#'
#' y <- stats::arima.sim(model = list("ar" = 0.6), n = 100)
#' # Adcf values are larger for initial lags.
#' adcf(y)
adcf <-
    function(x, lags = NULL, mu = c("szekely", "gaussian"), ...) {
        if (is.null(lags)) {
            lags <- 1:vctrs::vec_size(x) - 1
        }
        my_lags <- lags
        if (!(0 %in% lags)) {
            # add zero to lags parameter
            my_lags <- c(0, lags)
        }
        adcv_raw <-
            # compute auto-distance covariances
            dcf(x, x, my_lags, mu, ...) |>
            dplyr::pull("dcov")
        adcf_raw <-
            # convert to auto-distance correlations
            adcv_raw / adcv_raw[1]
        tibble::tibble(
            lag = my_lags,
            auto_dist_covariance = adcv_raw,
            auto_dist_correlation = adcf_raw
        ) |>
            # return required lags
            dplyr::filter(lag %in% lags)
    }