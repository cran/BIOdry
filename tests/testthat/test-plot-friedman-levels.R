## Regression test: with levels coarser than the series (e.g. levels =
## 'plot'), plot.frametoFriedman drew one line through all points of a
## panel in data order, joining different cores. It now draws one line per
## series within each panel. The default plot (one panel per series) is
## unchanged.
## Bundled data only: Prings05, Pradii03 (ringApply help route).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

ff <- Prings05 |>
    ringApply(lv = 2, fn = "rtimes") |>
    ringApply(lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum") |>
    frametoFriedman()

test_that("levels = 'plot' draws one line per core in each panel", {
    tr <- plot(ff, levels = "plot", layout = c(2, 1), print.trellis = FALSE)
    g <- tr$panel.args.common$groups
    expect_false(is.null(g))
    expect_length(tr$panel.args, 2L)
    for (k in seq_along(tr$panel.args)) {
        sub <- tr$panel.args[[k]]$subscripts
        gk <- droplevels(g[sub])
        ## four cores per plot, each with its time points in order
        expect_equal(nlevels(gk), 4L)
        x <- tr$panel.args[[k]]$x
        expect_true(all(tapply(x, gk, function(v) !is.unsorted(v))))
    }
    expect_setequal(levels(droplevels(g)),
                    c("a/7", "b/7", "a/17", "b/17", "a/18", "b/18", "a/56", "b/56"))
})

test_that("the default plot keeps one panel per core and no groups", {
    tr <- plot(ff, print.trellis = FALSE)
    expect_length(tr$panel.args, 8L)
    expect_null(tr$panel.args.common$groups)
})
