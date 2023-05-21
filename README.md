
# adcf

<!-- badges: start -->
<!-- badges: end -->

The goal of adcf is to calculate auto-distance covariance for univariate time series data.

## Installation

You can install the development version of adcf like so:

``` r
# Install development version from github
devtools::install_github("leonfernandes/adcf")
```

## Example

This is a basic example which shows you how to calculate auto distance covariance.

``` r
library(adcf)
# Simulate a univariate autoregressive time series
x <- stats::arima.sim(model = list("ar" = 0.5), n = 10)
adcf(x)
```