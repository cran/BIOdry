## Regression test: frametoLme's fitting copy (fit$model$data) must keep the
## time columns (time, year) even when the model uses only one of them, so
## that a refit with tdForm can use either. BIOdry 0.8's example refits the
## year model of modelFrame(Pchron) with log.t = TRUE on log(time); on the
## slim copy, which had no 'time', tdForm fell back to log(year).
## Bundled data only: Pchron (modelFrame example of BIOdry 0.8).
data(Pchron, package = "BIOdry", envir = environment())

## The fits use the single-level random parts of helper-fits.R; on the
## bundled data the default three-level fit is near singular.
test_that("0.8's refit with log.t = TRUE fits log(time) on fit$model$data", {
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    trwf <- modelFrame(Pchron, to = "cm",
                       MoreArgs = list(mp = c(2, 1, biom_param)),
                       form = "log(x) ~ log(csx) + year | tree",
                       random = nlme::pdDiag(~ 1))
    ## the year model keeps all 1008 rows; time is kept but not fitted
    expect_equal(trwf$model$dims$N, 1008L)
    expect_true(all(c("time", "year") %in% names(trwf$model$data)))
    expect_equal(deparse(tdForm(trwf$model$data, log.t = TRUE)),
                 "log(x) ~ log(csx) + log(time) | tree")
    rlme <- fit_tree(trwf$model$data, "log(time)", method = "ML")
    expect_equal(deparse(formula(rlme$model)), "log(x) ~ log(csx) + log(time)")
    ## rows without a time value are dropped only when time is fitted
    expect_equal(rlme$model$dims$N, 992L)
})
