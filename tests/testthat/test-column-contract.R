## Regression tests for the column contract (BIOdry 0.9.5): rtimes, scacum,
## moveYr and wlai find their columns by the package's names year, month and
## time (and hydro_year_id from moveYr). Aliases such as yr, calendar_year,
## hydro_year, water_year, mon, mnt or mo, added in development versions
## 0.9.2-0.9.4, are no longer read.
## Bundled data only: Prings05, PTclim05. To test an alias, one column is
## renamed (and, for the month, coded as 1-12); no other value changes.
data(Prings05, package = "BIOdry", envir = environment())
data(PTclim05, package = "BIOdry", envir = environment())

rename_col <- function(d, from, to) {
    names(d)[names(d) == from] <- to
    d
}

test_that("moveYr requires a column named year", {
    expect_error(moveYr(rename_col(PTclim05, "year", "yr")),
                 "requires a column named 'year'", fixed = TRUE)
    expect_equal(dim(moveYr(PTclim05)), c(1224L, 6L))
})

test_that("rtimes does not read a year column named yr, and warns", {
    expect_equal(attr(rtimes(Prings05), "time_vars"), "year")
    expect_warning(r <- rtimes(rename_col(Prings05, "year", "yr")),
                   "none of the columns 'year', 'hydro_year_id', 'month' or 'time'",
                   fixed = TRUE)
    expect_length(attr(r, "time_vars"), 0L)
})

test_that("scacum does not order or label by a year column named yr", {
    d <- ringApply(Prings05, lv = 2, fn = "rtimes")
    a <- scacum(d, sc.c = 100, rf.t = 2003, lv = "sample")
    b <- scacum(rename_col(d, "year", "yr"), sc.c = 100, rf.t = 2003, lv = "sample")
    expect_true("year" %in% attr(a, "time_vars"))
    expect_false("yr" %in% attr(b, "time_vars"))
})

test_that("wlai does not take a numeric column named mnt as the month", {
    d <- PTclim05
    d$mnt <- match(as.character(d$month), month.abb)
    d$month <- NULL
    with_mnt <- as.data.frame(wlai(d))
    without <- as.data.frame(wlai(d[, setdiff(names(d), "mnt")]))
    expect_equal(with_mnt$AI, without$AI)
    ## the package's month column is still found by name or by month names
    expect_equal(nrow(as.data.frame(wlai(PTclim05))), 102L)
})

test_that("scacum stops when rf.t is given and there is no year or time column", {
    p <- rename_col(Prings05, "year", "yr")
    expect_error(scacum(p, sc.c = 100, rf.t = 2003, lv = "sample"),
                 "'rf.t' was given", fixed = TRUE)
    ## without rf.t, cumulative sums are still computed
    expect_equal(nrow(scacum(p, lv = "sample")), nrow(Prings05))
})

test_that("wlai warns when none of the contract columns is present", {
    d <- rename_col(PTclim05, "year", "yr")
    d$mnt <- match(as.character(d$month), month.abb)
    d$month <- NULL
    expect_warning(wlai(d),
                   "none of the columns 'year', 'hydro_year_id', 'month' or 'time'",
                   fixed = TRUE)
    ## no warning on the bundled data or on the help example's row names
    expect_silent(wlai(PTclim05))
    cld <- data.frame(pr = c(30, 40, 50, 60, 55, 45, 20, 10, 15, 35, 45, 50),
                      tm = c(10, 8, 6, 5, 7, 10, 14, 18, 22, 25, 20, 15))
    rownames(cld) <- c(10:12, 1:9)
    expect_silent(wlai(cld))
})
