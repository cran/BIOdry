## Regression test: update() of a frametoLme fit stores the original data
## expression in the call (it showed the internal placeholder ..rd..), and
## the refit equals a direct fit with the same arguments.
## Bundled data only: Prings05, Pradii03 (frametoLme help route).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

test_that("update(fit, method = 'ML') shows the original data name", {
    dfm2 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    dfm2 <- ringApply(dfm2, lv = "sample", sc.c = Pradii03, rf.t = 2003,
                      fn = "scacum")
    fit <- frametoLme(dfm2, form = "log(x) ~ log(csx) + log(time) | tree",
                      random = pdDiag(~ log(csx)))
    fit_ml <- update(fit, method = "ML")
    expect_identical(fit_ml$call[[2L]], as.name("dfm2"))
    out <- paste(capture.output(print(fit_ml)), collapse = "\n")
    expect_match(out, "frametoLme(dfm2,", fixed = TRUE)
    expect_false(grepl("..rd..", out, fixed = TRUE))
    ## a refit of the refit also keeps it
    fit_t <- update(fit_ml, form = "log(x) ~ log(csx) + time | tree")
    expect_identical(fit_t$call[[2L]], as.name("dfm2"))

    fit_dir <- frametoLme(dfm2, form = "log(x) ~ log(csx) + log(time) | tree",
                          random = pdDiag(~ log(csx)), method = "ML")
    expect_equal(fixef(fit_ml$model), fixef(fit_dir$model))
    expect_equal(as.numeric(logLik(fit_ml$model)),
                 as.numeric(logLik(fit_dir$model)))
})
