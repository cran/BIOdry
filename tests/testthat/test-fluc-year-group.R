## Regression test: when year is both a grouping level and a time column,
## $fluc must carry it once. On the declared-year route the residual frame
## had columns year.res, year, time and a duplicate year.1.
## Bundled data only: Prings05, Pradii03 (ringApply help route).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

test_that("$fluc carries year once when year is the grouping level", {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    dfm2 <- ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003,
                      fn = "scacum")
    dr <- defineDataRoles(dfm2, time = "time",
                          groups = c("year", "plot", "tree", "sample"))
    ## a random intercept per year is well posed on these data; the
    ## default random part would be near singular
    fit <- frametoLme(dr, form = "log(x) ~ log(csx) + time | year",
                      random = nlme::pdDiag(~ 1))
    fl <- as.data.frame(fit$fluc)
    expect_false("year.1" %in% names(fl))
    expect_equal(anyDuplicated(names(fl)), 0L)
    expect_true(all(c("year.res", "year", "time") %in% names(fl)))
    ## year.res holds the Pearson residuals of the fit, row by row
    r <- residuals(fit$model, level = c(1, 0), type = "pearson")
    expect_equal(fl$year.res, unname(r[rownames(fl), "year"]))
})
