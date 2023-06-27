#' Auto distance covariance
#'
#' Calculates the auto-distance covariance/correlation of `x`.
#'
#' @param x numeric vector.
#' @param lag integer. Specifies lag(s) at which distance covariance is to be
#'      calculated. When `NULL` maximum range of lags are used.
#' @param mu Single character string corresponding to the weight measure to be
#'      used. Must be one of "szekely" or "gaussian".
#' @param ... Optional parameters supplied for the weight measure `mu`.
#' @export
#' @returns An object of class `adcf_tbl`.
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
    function(x, lag = NULL, mu = c("szekely", "gaussian"), ...) {
        if (is.null(lag)) {
            lag <- 1:vctrs::vec_size(x) - 1
        }
        if (!(0 %in% lag)) {
            lag <- c(0, lag)
        }
        ret <- adcv(x, lag, mu, ...) |>
            adcv_to_adcf() |>
            as.list()
        tibble::new_tibble(ret, class = "adcf_tbl")
    }

#' @rdname adcf
#' @export
adcv <-
    function(x, lag = NULL, mu = c("szekely", "gaussian"), ...) {
        if (is.null(lag)) {
            lag <- 2:vctrs::vec_size(x) - 1
        }
        value <-
            dcf(x, x, lag, mu, ...) |>
            dplyr::rename(adcv = dcov)
        tibble::new_tibble(value, class = "adcv_tbl")
    }