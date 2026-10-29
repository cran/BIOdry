## Regression test: refitting a frametoLme fit's model data must give the
## same nesting as the original fit. The slim fitting copy put the grouping
## columns after the formula variables (x csx time plot tree sample), and
## tdForm, which takes the outer level first as the reverse of the column
## order, then built sample/tree/plot.
## Bundled data only: Prings05, Pradii03 (ringApply help route).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

test_that("refitting fit$model$data with tdForm keeps plot/tree/sample", {
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = "rtimes") |>
        ringApply(lv = "sample", sc.c = Pradii03, rf.t = 2003,
                  fn = "scacum")
    fit <- frametoLme(dfm2, form = "tdForm")
    expect_equal(names(fit$model$groups), c("plot", "tree", "sample"))
    refit <- frametoLme(fit$model$data, form = "tdForm", on.time = FALSE)
    expect_equal(names(refit$model$groups), c("plot", "tree", "sample"))
    ## the grouping columns of the slim copy keep the input's order
    g <- c("plot", "tree", "sample")
    expect_equal(intersect(names(fit$model$data), g),
                 intersect(names(dfm2), g))
})
