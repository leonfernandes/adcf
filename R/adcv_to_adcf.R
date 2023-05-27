#' Convert auto distance covariance to auto distance correlation
#'
#' @param object a tibble of class `adcv_tbl` that includes row with auto
#'      ditance covariance at lag 0.
#' @export
#' @return tibble of class `adcf_tbl`.
adcf_to_adcv <- function(object) {
    if (!inherits(object, "adcv_tbl")) {
        stop("`object` is not of class `adcf_tbl.")
    }
    if (!(0 %in% object$lag)) {
        stop("lag 0 value not available.")
    }
    value_lag0 <-
        object |>
        dplyr::filter(lag == 0) |>
        dplyr::pull("adcv")
    ret <-
        object |>
        dplyr::filter(lag != 0) |>
        dplyr::mutate(adcv = adcv / value_lag0) |>
        dplyr::rename(adcf = adcv)
    tibble::new_tibble(ret, "adcf_tbl")
}