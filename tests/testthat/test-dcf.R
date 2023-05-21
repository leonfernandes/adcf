test_that("dcf_energy works", {
    set.seed(6991)
    x <- rnorm(1000)
    y <- rcauchy(100)
    expect_snapshot(
        dcf(x, y)
    )
})