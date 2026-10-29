## Regression test: muleMan() must label each correlogram with the plot, tree
## and core it belongs to, as BIOdry 0.9.1 did (e.g. plot = P16106,
## tree = 17, sample = a), whatever the column order of the input.
## Bundled data only: Pchron, Prec, Temp (muleMan help example).
data(Pchron, package = "BIOdry", envir = environment())
data(Prec, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())

biomass_fluc <- function() {
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    modelFrame(Pchron, to = "cm", MoreArgs = list(mp = c(2, 1, biom_param)),
               log.t = FALSE, on.time = FALSE)
}
aridity_fluc <- function() {
    modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
               lv = list("year", "year"), form = "lmeForm")
}
lab <- function(d) paste(d$plot, d$tree, d$sample)

test_that("muleMan labels P16106 tree 17 core a as plot/tree/sample", {
    trwf <- biomass_fluc()
    aif <- aridity_fluc()
    out <- as.data.frame(muleMan(trwf, aif)$mmgram)
    expect_true("P16106 17 a" %in% lab(out))
    expect_setequal(as.character(unique(out$plot)), c("P16106", "P44005"))
    expect_setequal(as.character(unique(out$sample)), c("a", "b"))
    ## every core of the tree-ring data appears, under its own labels
    fl <- as.data.frame(trwf$fluc)
    expect_setequal(unique(lab(out)), unique(lab(fl)))
})

test_that("reordering the tree-ring grouping columns gives the same labels", {
    trwf <- biomass_fluc()
    aif <- aridity_fluc()
    fl <- as.data.frame(trwf$fluc)
    grp <- c("sample", "tree", "plot")
    fl_reordered <- fl[, c(setdiff(names(fl), grp), grp)]
    expect_equal(names(fl_reordered)[(ncol(fl) - 2):ncol(fl)], grp)
    a <- as.data.frame(muleMan(trwf, aif)$mmgram)
    b <- as.data.frame(muleMan(fl_reordered, aif)$mmgram)
    key <- function(d) paste(lab(d), signif(d$lag, 10))
    expect_setequal(key(a), key(b))
    i <- match(key(a), key(b))
    expect_equal(b$mantelr[i], a$mantelr, tolerance = 1e-10)
    expect_true("P16106 17 a" %in% lab(b))
})
