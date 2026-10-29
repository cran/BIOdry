## Regression test: the fixed-effects table of a printed frametoLme summary
## must read like nlme's summary: no scientific notation for ordinary
## values, and DF as whole numbers. printCoefmat put every column in one
## format, so DF printed as 9.8200e+02.
## Bundled data only: Prings05, Pradii03 (ringApply help route).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

fixed_table <- function(out) {
    i <- which(out == "Fixed effects:")
    j <- i + which(out[(i + 1L):length(out)] == "")[1L] - 1L
    read.table(text = out[(i + 1L):j], header = TRUE, check.names = FALSE,
               colClasses = "character")
}

test_that("the printed fixed-effects table has whole DF and plain values", {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    dfm2 <- ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003,
                      fn = "scacum")
    fit <- frametoLme(dfm2, form = "tdForm")
    out <- capture.output(print(fit))
    tab <- fixed_table(out)
    expect_equal(rownames(tab), c("(Intercept)", "log(csx)", "time"))
    expect_false(any(grepl("e[+-]", tab$DF)))
    expect_false(any(grepl("e[+-]", unlist(tab))))
    df <- as.numeric(tab$DF)
    expect_equal(df, round(df))
    expect_equal(df, unname(summary(fit$model)$tTable[, "DF"]))
    ## the coefficients themselves are unchanged
    expect_equal(summary(fit)$fixed, summary(fit$model)$tTable)
    expect_equal(as.numeric(tab$Value), unname(nlme::fixef(fit$model)),
                 tolerance = 1e-4)
})
