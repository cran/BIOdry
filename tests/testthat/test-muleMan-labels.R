## Regression test: muleMan() must label each correlogram with the plot, tree
## and core it belongs to, as BIOdry 0.9.1 did (e.g. plot = P16106,
## tree = 17, sample = a), whatever the column order of the input.
## Bundled data only: Pchron, Prec, Temp (muleMan help example), as the
## processed series of helper-fits.R (no fit: on the bundled data the
## default three-level fit is near singular).
lab <- function(d) paste(d$plot, d$tree, d$sample)

test_that("muleMan labels P16106 tree 17 core a as plot/tree/sample", {
    trw <- trw_series()
    out <- as.data.frame(muleMan(trw, ai_series(), rd.var = "x",
                                 cd.var = "AI", nperm = 0)$mmgram)
    expect_true("P16106 17 a" %in% lab(out))
    expect_setequal(as.character(unique(out$plot)), c("P16106", "P44005"))
    expect_setequal(as.character(unique(out$sample)), c("a", "b"))
    ## every core of the tree-ring data appears, under its own labels
    expect_setequal(unique(lab(out)), unique(lab(trw)))
})

test_that("reordering the tree-ring grouping columns gives the same labels", {
    fl <- trw_series()
    ai <- ai_series()
    grp <- c("plot", "tree", "sample")
    expect_equal(names(fl)[(ncol(fl) - 2):ncol(fl)], grp)
    fl_reordered <- fl[, c(setdiff(names(fl), grp), rev(grp))]
    expect_equal(names(fl_reordered)[(ncol(fl) - 2):ncol(fl)], rev(grp))
    a <- as.data.frame(muleMan(fl, ai, rd.var = "x", cd.var = "AI",
                               nperm = 0)$mmgram)
    b <- as.data.frame(muleMan(fl_reordered, ai, rd.var = "x",
                               cd.var = "AI", nperm = 0)$mmgram)
    key <- function(d) paste(lab(d), signif(d$lag, 10))
    expect_setequal(key(a), key(b))
    i <- match(key(a), key(b))
    expect_equal(b$mantelr[i], a$mantelr, tolerance = 1e-10)
    expect_true("P16106 17 a" %in% lab(b))
})
