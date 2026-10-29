## Regression tests for the column-role rule (BIOdry 0.9.5): roles come from
## column types, as in BIOdry 0.9.1 (cClass). Factor and character columns
## are groups, whole-number numeric columns are time candidates, other
## numeric columns are responses. The month column (named 'month', or a
## factor whose levels are month names) is never a group. Roles declared
## with defineDataRoles() override the rule; metadata that ringApply()
## creates automatically does not.
## Bundled data only: Prings05, Pradii03, PTclim05. The month-rule test
## recodes tree as a factor with levels 1-12; the factor-copy route adds
## factor(year), as agreed for these two checks.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())
data(PTclim05, package = "BIOdry", envir = environment())

rings_rtimes_scacum <- function() {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

test_that("PTclim05: month is not a grouping column", {
    expect_equal(deparse(lmeForm(PTclim05)), "pr ~ year | plot")
    expect_length(slitFrame(PTclim05), 2L)
    expect_equal(attr(defineDataRoles(PTclim05), "BIOdry_meta")$groups, "plot")
})

test_that("moveYr output: the metadata groups are plot only", {
    cl1 <- ringApply(PTclim05, lv = "year", fn = "moveYr")
    m <- attr(cl1, "BIOdry_meta")
    expect_equal(m$groups, "plot")
    expect_equal(m$role_hints$groups, "plot")
})

test_that("wlai output: the time hint is year, the model's covariate", {
    cl1 <- ringApply(PTclim05, lv = "year", fn = "moveYr")
    wl <- ringApply(cl1, lv = "year", fn = "wlai")
    m <- attr(wl, "BIOdry_meta")
    expect_equal(m$role_hints$time, "year")
    expect_equal(m$groups, "plot")
    expect_equal(deparse(lmeForm(wl)), "AI ~ year | plot")
})

test_that("a factor with levels 1-12 that is not named month stays a group", {
    d <- ringApply(Prings05, lv = 2, fn = "rtimes")
    d$tree <- factor(as.integer(d$tree), levels = 1:12)
    expect_true("tree" %in% cClass(d, "factor"))
    expect_equal(deparse(lmeForm(d)), "x ~ year | plot/tree/sample")
    expect_equal(deparse(tdForm(d)), "log(x) ~ log(csx) + time | plot/tree/sample")
    expect_length(slitFrame(d), 8L)
})

test_that("year declared as a group can be a random grouping level", {
    dr <- defineDataRoles(rings_rtimes_scacum(), time = "time",
                          groups = c("year", "plot", "tree", "sample"))
    fit <- frametoLme(dr, form = "log(x) ~ log(csx) + time | year")
    expect_equal(names(fit$model$groups), "year")
    expect_equal(nlevels(fit$model$groups$year), 142L)
    expect_equal(as.numeric(logLik(fit$model)), -770.621064, tolerance = 1e-6)
    expect_equal(deparse(lmeForm(dr)), "x ~ time | year/plot/tree/sample")
})

test_that("a factor copy of year is a group and year stays the covariate", {
    d2 <- rings_rtimes_scacum()
    d2$fyear <- factor(d2$year)
    fit <- frametoLme(d2, form = "log(x) ~ log(csx) + year | fyear")
    expect_equal(names(fit$model$groups), "fyear")
    expect_true("year" %in% names(nlme::fixef(fit$model)))
    expect_equal(as.numeric(logLik(fit$model)), -874.420881, tolerance = 1e-6)
    ## automatic ringApply metadata does not hide the new factor column
    expect_equal(deparse(lmeForm(d2)), "x ~ year | fyear/plot/tree/sample")
})
