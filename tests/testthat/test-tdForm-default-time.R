## Regression test: by default tdForm() uses the relative time column from
## rtimes() ('time') when it is present, as BIOdry 0.9.1 did; 'year' is used
## when there is no relative time column, when on.time = FALSE, or when the
## user declares it with defineDataRoles(time = "year").
## Bundled data only: Prings05, Pradii03.
##
## Reference values: the data frame produced by this package's ringApply
## help route (rtimes, then scacum with sc.c = Pradii03, rf.t = 2003; the
## scacum of 0.9.1 itself was wrong, see test-scacum-own-radius.R) was saved
## as a plain data frame and passed to CRAN BIOdry 0.9.1:
##   d <- as.data.frame(rings_rtimes_scacum()); attr(d, "BIOdry_meta") <- NULL
##   ## in a session with 0.9.1 loaded from its own library:
##   f <- frametoLme(d, form = "tdForm")
##   deparse(formula(f$model)); logLik(f$model); nlme::fixef(f$model)
ref_091 <- list(
    formula = "log(x) ~ log(csx) + time",
    logLik = -712.0308023459,
    fixef = c(7.56730237144, -1.79983694733, 0.000458368427443)
)

data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

rings_rtimes_scacum <- function(rd = Prings05) {
    dfm1 <- ringApply(rd, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

test_that("the default tdForm on the ringApply help route uses 'time', as 0.9.1", {
    fit <- frametoLme(rings_rtimes_scacum(), form = "tdForm")
    expect_equal(deparse(formula(fit$model)), ref_091$formula)
    expect_equal(as.numeric(logLik(fit$model)), ref_091$logLik, tolerance = 1e-6)
    expect_equal(unname(nlme::fixef(fit$model)), ref_091$fixef, tolerance = 1e-6)
})

test_that("on.time = FALSE uses 'year'", {
    fit <- frametoLme(rings_rtimes_scacum(), form = "tdForm", on.time = FALSE)
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + year")
})

test_that("a year declared with defineDataRoles is used", {
    rd <- defineDataRoles(Prings05, response = "x", time = "year",
                          groups = "plot/tree/sample")
    fit <- frametoLme(rings_rtimes_scacum(rd), form = "tdForm")
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + year")
})

test_that("a time variable passed to defineDataRoles by position counts as declared", {
    ## defineDataRoles(rd, inputs, response, time, ...): 'year' in 4th place
    rd <- defineDataRoles(Prings05, NULL, "x", "year", "plot/tree/sample")
    expect_equal(as.list(attr(rd, "BIOdry_meta")$call)[["time"]], "year")
    fit <- frametoLme(rings_rtimes_scacum(rd), form = "tdForm")
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + year")
    ## declared on the help-route output, where 'time' would otherwise be used
    rd2 <- defineDataRoles(rings_rtimes_scacum(), NULL, "x", "year", "plot/tree/sample")
    fit2 <- frametoLme(rd2, form = "tdForm")
    expect_equal(deparse(formula(fit2$model)), "log(x) ~ log(csx) + year")
})

test_that("a formula string with 'year' is used as given", {
    fit <- frametoLme(rings_rtimes_scacum(),
                      form = "log(x) ~ log(csx) + year | plot/tree/sample")
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + year")
})
