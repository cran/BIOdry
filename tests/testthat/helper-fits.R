## Fits shared by the tests.
## The bundled tree-ring data are 2 plots x 2 trees x 2 cores. On them every
## plot/tree/sample random part, the default one included, is singular or
## close to it: the plot and core variances go to about zero, and the
## estimates move with the optimiser and the BLAS (CRAN r-release-macos-x86_64
## and MKL checks of 0.9.5). The tests below therefore fit only single-level
## random parts whose estimates agree between the nlminb and optim
## optimisers of nlme:
##   tree-ring data, time or log(time): '| tree', random = pdDiag(~ log(csx))
##   tree-ring data, year:              '| tree', random = pdDiag(~ 1)
##   climate data, AI ~ year:           '| plot', random = pdDiag(~ 1)
## Checks of the three-level default route are in
## test-default-route-3-levels.R and do not run on CRAN.

## tree-ring model with tree as the only grouping level; 'covariate' is the
## time term of the Zeide form
fit_tree <- function(rd, covariate = "log(time)", ...) {
    random <- if (covariate == "year") nlme::pdDiag(~ 1)
              else nlme::pdDiag(~ log(csx))
    frametoLme(rd, form = paste("log(x) ~ log(csx) +", covariate, "| tree"),
               random = random, ...)
}

## climate model of the aridity index with a random intercept per plot
fit_climate <- function(rd, ...) {
    frametoLme(rd, form = "AI ~ year | plot", random = nlme::pdDiag(~ 1), ...)
}

## muleMan inputs without a fit: the processed series of the modelFrame and
## muleMan help routes (Pchron with the biomass parameters of the modelFrame
## example; Prec and Temp through moveYr and wlai), reduced to the columns a
## $fluc table carries, in its order: the series, year, plot, tree, sample
## (muleMan nests its output by the column order of the grouping columns).
## Each series is computed once per test run; every call returns a copy.
series_cache <- new.env()
trw_series <- function() {
    if (is.null(series_cache$trw)) {
        data(Pchron, package = "BIOdry", envir = environment())
        d <- modelFrame(Pchron, form = NULL, to = "cm",
                        MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)))
        series_cache$trw <- d[, c("x", "year", "plot", "tree", "sample")]
    }
    series_cache$trw
}
ai_series <- function() {
    if (is.null(series_cache$ai)) {
        data(Prec, package = "BIOdry", envir = environment())
        data(Temp, package = "BIOdry", envir = environment())
        d <- modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
                        lv = list("year", "year"), form = NULL)
        series_cache$ai <- d[, c("AI", "year", "plot")]
    }
    series_cache$ai
}
