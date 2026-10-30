## Regression test: plot() of a frametoLme fit made with res.data = FALSE must
## rebuild the residual fluctuations from the model and plot them; it failed
## with "subscript out of bounds" on the one-sided grouping formula.
## Bundled data only: Prings05, Pradii03, Prec, Temp.
## The fits use the single-level random parts of helper-fits.R; the
## three-level default route is checked in test-default-route-3-levels.R.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())
data(Prec, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())

rings_rtimes_scacum <- function() {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

## points of each panel, keyed by the panel's strip levels
panel_points <- function(tr) {
    lev <- do.call(expand.grid, c(tr$condlevels, stringsAsFactors = FALSE))
    keys <- apply(lev, 1L, paste, collapse = "|")
    out <- lapply(seq_along(tr$panel.args), function(k) {
        p <- tr$panel.args[[k]]
        o <- order(p$x, p$y)
        data.frame(x = as.numeric(p$x[o]), y = as.numeric(p$y[o]))
    })
    names(out) <- keys[seq_along(out)]
    out[order(names(out))]
}

test_that("plot() works on a fit without $fluc and shows the same points", {
    rd <- rings_rtimes_scacum()
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    with_fluc <- plot(fit_tree(rd, "time"), print_trellis = FALSE)
    without <- plot(fit_tree(rd, "time", res.data = FALSE),
                    print_trellis = FALSE)
    expect_s3_class(without, "trellis")
    a <- panel_points(with_fluc)
    b <- panel_points(without)
    expect_equal(names(b), names(a))
    expect_equal(b, a, tolerance = 1e-10)
})

test_that("plot() works without $fluc on a one-level model", {
    d <- modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
                    lv = list("year", "year"), form = NULL)
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    tr <- plot(fit_climate(d, res.data = FALSE), print_trellis = FALSE)
    expect_s3_class(tr, "trellis")
})
