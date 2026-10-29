## Regression test: scacum() must scale every core to its own tree's radius.
## Bundled data only: Prings05 (ring widths), Pradii03 (inside-bark radii of
## the four trees measured in 2003), through the ringApply help example
## (tree-level rtimes, then sample-level scacum with sc.c = Pradii03 and
## rf.t = 2003). In BIOdry 0.9.1 some cores were scaled to another tree's
## radius.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

scaled_rings <- function() {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    d <- as.data.frame(ringApply(dfm1, lv = "sample", sc.c = Pradii03,
                                 rf.t = 2003, fn = "scacum"))
    d$core <- paste(d$plot, d$tree, d$sample)
    d$tree_id <- paste(d$plot, d$tree, sep = ".")
    d
}

test_that("in 2003 every core equals its own tree's radius in Pradii03", {
    d <- scaled_rings()
    d03 <- d[d$year == 2003, ]
    expect_equal(nrow(d03), 8L)
    expect_setequal(d03$tree_id, names(Pradii03))
    expect_equal(d03$csx, unname(Pradii03[d03$tree_id]), tolerance = 1e-10)
})

test_that("every core's series is its tree's radius shifted by its own rings", {
    ## expected csx in year t: radius + cumsum(x) up to t - cumsum(x) up to 2003
    d <- scaled_rings()
    for (cr in unique(d$core)) {
        dc <- d[d$core == cr, ]
        dc <- dc[order(dc$year), ]
        cs <- cumsum(dc$x)
        expected <- unname(Pradii03[dc$tree_id[1L]]) + cs - cs[dc$year == 2003]
        expect_equal(dc$csx, expected, tolerance = 1e-10, info = cr)
    }
})
