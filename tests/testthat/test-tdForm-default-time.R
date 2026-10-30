## Regression test: by default tdForm() uses the relative time column from
## rtimes() ('time') when it is present, as BIOdry 0.9.1 did; 'year' is used
## when there is no relative time column, when on.time = FALSE, or when the
## user declares it with defineDataRoles(time = "year").
## Bundled data only: Prings05, Pradii03.
##
## The formulas are checked without fitting: on the bundled data the default
## three-level fit is near singular (see helper-fits.R). That frametoLme()
## with form = "tdForm" fits these formulas is checked in
## test-default-route-3-levels.R. Reference: CRAN BIOdry 0.9.1 fitted
## log(x) ~ log(csx) + time on the data of this package's ringApply help
## route (rtimes, then scacum with sc.c = Pradii03, rf.t = 2003).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

rings_rtimes_scacum <- function(rd = Prings05) {
    dfm1 <- ringApply(rd, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

test_that("the default tdForm on the ringApply help route uses 'time', as 0.9.1", {
    expect_equal(deparse(tdForm(rings_rtimes_scacum())),
                 "log(x) ~ log(csx) + time | plot/tree/sample")
})

test_that("on.time = FALSE uses 'year'", {
    expect_equal(deparse(tdForm(rings_rtimes_scacum(), on.time = FALSE)),
                 "log(x) ~ log(csx) + year | plot/tree/sample")
})

test_that("a year declared with defineDataRoles is used", {
    rd <- defineDataRoles(Prings05, response = "x", time = "year",
                          groups = "plot/tree/sample")
    expect_equal(deparse(tdForm(rings_rtimes_scacum(rd))),
                 "log(x) ~ log(csx) + year | plot/tree/sample")
})

test_that("a time variable passed to defineDataRoles by position counts as declared", {
    ## defineDataRoles(rd, inputs, response, time, ...): 'year' in 4th place
    rd <- defineDataRoles(Prings05, NULL, "x", "year", "plot/tree/sample")
    expect_equal(as.list(attr(rd, "BIOdry_meta")$call)[["time"]], "year")
    expect_equal(deparse(tdForm(rings_rtimes_scacum(rd))),
                 "log(x) ~ log(csx) + year | plot/tree/sample")
    ## declared on the help-route output, where 'time' would otherwise be used
    rd2 <- defineDataRoles(rings_rtimes_scacum(), NULL, "x", "year", "plot/tree/sample")
    expect_equal(deparse(tdForm(rd2)),
                 "log(x) ~ log(csx) + year | plot/tree/sample")
})

test_that("a formula string with 'year' is used as given", {
    fit <- fit_tree(rings_rtimes_scacum(), "year")
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + year")
})
