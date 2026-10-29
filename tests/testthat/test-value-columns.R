## Regression tests for the value and climate columns (BIOdry 0.9.5):
## scacum's value column is x, wlai's precipitation and temperature are pr
## and tm (the package's column contract, as in BIOdry 0.9.1). Roles declared
## with defineDataRoles() take precedence. Name lists such as trw/rw/rwi/abi
## or prec/ppt/temp, added in development versions 0.9.2-0.9.4, are gone.
## Bundled data only: Prings05, PTclim05; a column is renamed only to
## trigger a case, values are not changed.
data(Prings05, package = "BIOdry", envir = environment())
data(PTclim05, package = "BIOdry", envir = environment())

rename_col <- function(d, from, to) {
    names(d)[names(d) == from] <- to
    d
}

test_that("scacum stops when there is no x and nothing declared", {
    p <- rename_col(Prings05, "x", "rw")
    expect_error(scacum(p, lv = "sample"), "no column 'x'", fixed = TRUE)
})

test_that("scacum uses a response declared with defineDataRoles", {
    p <- defineDataRoles(rename_col(Prings05, "x", "rw"), response = "rw",
                         validate = "none")
    expect_equal(scacum(p, lv = "sample")$csx,
                 scacum(Prings05, lv = "sample")$csx)
})

test_that("wlai without pr and tm warns and uses the first two numeric columns", {
    d <- rename_col(rename_col(PTclim05, "pr", "prec"), "tm", "temp")
    expect_warning(a <- wlai(d),
                   "using 'prec' as precipitation and 'temp' as temperature",
                   fixed = TRUE)
    expect_equal(as.data.frame(a)$AI, as.data.frame(wlai(PTclim05))$AI)
})

test_that("wlai uses inputs declared with defineDataRoles, without a warning", {
    d <- rename_col(rename_col(PTclim05, "pr", "prec"), "tm", "temp")
    d <- defineDataRoles(d, inputs = c("prec", "temp"), validate = "none")
    expect_silent(a <- wlai(d))
    expect_equal(as.data.frame(a)$AI, as.data.frame(wlai(PTclim05))$AI)
})

test_that("wlai uses declared pr and tm by name whatever their order", {
    d <- defineDataRoles(PTclim05, inputs = c("tm", "pr"), validate = "none")
    expect_silent(a <- wlai(d))
    expect_equal(as.data.frame(a)$AI, as.data.frame(wlai(PTclim05))$AI)
})
