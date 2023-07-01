#' Calculate pairwise distance covariances
#'
#' Flexible version of distance covariance which uses the maximum number of
#' observations from `x` and `y` for each lags.
#'
#' @param x numeric vector.
#' @param y numeric vector.
#' @param lags integer. Lags to be applied to `y`.
#' @param mu Single character string corresponding to the weight measure to be
#'      used. Must be one of "szekely" or "gaussian".
#' @param ... Optional parameters supplied for the weight measure `mu`.
#' @useDynLib adcf
#' @export
#' @returns A tibble of lagged distance covariances.
dcf <-
    function(
        x, y, lags = 0:1, mu = c("szekely", "gaussian"), ...
    ) {
        mu <- match.arg(mu)
        # Determine if dcov::dcov can be used
        use_szekely_fast <- (
            mu == "szekely" &&
                (is.null(list(...)[["index"]]) || list(...)[["index"]] == 1)
        )
        # Define dcov_calc using either dcov::dcov or adcf implementation
        if (use_szekely_fast) {
            dcov_calc <- function(xx, yy) {
                dcov::dcov(xx, yy)
            }
        } else {
            mu_hat <- get_weight_measure(mu, ...)
            dcov_calc <- function(xx, yy) {
                xxx <- pairwise_dist(xx, mu_hat)
                yyy <- pairwise_dist(yy, mu_hat)
                dist_to_dcov(xxx, yyy)
            }
        }
        # Calculate distance covariances for each lags
        nx <- vctrs::vec_size(x)
        ny <- vctrs::vec_size(y)
        dcov_vec <- purrr::map_dbl(
            lags,
            function(h) {
                if (h >= ny || h < 0) {
                    warning(
                        "Some lags(s) are not positive or are too large. NA's
                        produced."
                    )
                    return(NA_real_)
                }
                h_max <- min(ny - h, nx)
                dcov_calc(
                    vctrs::vec_slice(x, 1:h_max),
                    vctrs::vec_slice(y, 1:h_max + h)
                )
            }
        )
        tibble::tibble(lag = lags, dcov = dcov_vec)
    }