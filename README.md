
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

## Introduction

The goal of adcf is to calculate auto-distance covariance for univariate
time series data.

## Installation

You can install the development version of adcf like so:

``` r
# Install development version from github
pak::pkg_install("leonfernandes/adcf")
```

## Example

This is a basic example which shows you how to calculate auto distance
covariance.

``` r
library(adcf)
# Simulate a univariate autoregressive time series
x <- stats::arima.sim(model = list("ar" = 0.5), n = 10)
adcf(x)
#> # A tibble: 10 × 3
#>      lag auto_dist_covariance auto_dist_correlation
#>    <dbl>                <dbl>                 <dbl>
#>  1     0               0.236                 1     
#>  2     1               0.118                 0.499 
#>  3     2               0.0653                0.277 
#>  4     3               0.0498                0.211 
#>  5     4               0.0331                0.140 
#>  6     5               0.0734                0.312 
#>  7     6               0.0870                0.370 
#>  8     7               0.0252                0.107 
#>  9     8               0.0116                0.0492
#> 10     9               0                     0
```
