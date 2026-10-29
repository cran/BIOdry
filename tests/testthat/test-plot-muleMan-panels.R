## Regression test: plot.muleMan() orders the correlogram panels by plot,
## then tree (as numbers), then core, and labels the axes "Lag" and
## "Mantel r" unless xlab/ylab are given.
## Bundled data only: Pchron, Prec, Temp (muleMan help example).
data(Pchron, package = "BIOdry", envir = environment())
data(Prec, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())

mule_example <- function() {
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    trwf <- modelFrame(Pchron, to = "cm",
                       MoreArgs = list(mp = c(2, 1, biom_param)),
                       log.t = FALSE, on.time = FALSE)
    aif <- modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
                      lv = list("year", "year"), form = "lmeForm")
    set.seed(1)
    muleMan(trwf, aif, nperm = 100)
}

test_that("panels are ordered by plot, tree and core", {
    mm <- mule_example()
    pdf(NULL); on.exit(dev.off())
    tr <- plot(mm)
    expect_identical(as.character(tr$condlevels[[1]]),
                     c("a/17/P16106", "b/17/P16106", "a/18/P16106",
                       "b/18/P16106", "a/7/P44005", "b/7/P44005",
                       "a/56/P44005", "b/56/P44005"))
})

test_that("axes are labelled Lag and Mantel r unless given", {
    mm <- mule_example()
    pdf(NULL); on.exit(dev.off())
    tr <- plot(mm)
    expect_identical(tr$xlab, "Lag")
    expect_identical(tr$ylab, "Mantel r")
    tr2 <- plot(mm, xlab = "Distance class", ylab = "r")
    expect_identical(tr2$xlab, "Distance class")
    expect_identical(tr2$ylab, "r")
})

test_that("each panel keeps its own correlogram values", {
    mm <- mule_example()
    pdf(NULL); on.exit(dev.off())
    tr <- plot(mm)
    g <- as.data.frame(mm$mmgram)
    lab <- paste(g$sample, g$tree, g$plot, sep = "/")
    for (i in seq_along(tr$panel.args)) {
        p <- tr$panel.args[[i]]
        sel <- lab == as.character(tr$condlevels[[1]][i])
        expect_equal(sort(p$y), sort(g$mantelr[sel]))
    }
})
