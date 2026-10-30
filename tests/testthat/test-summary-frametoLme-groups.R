## Regression test: the summary of a frametoLme fit must list only the
## model's grouping levels. It listed nlme's dims$ngrps unfiltered, which
## also counts the model matrix and response ("X 1, y 1").
## Bundled data only: Prings05, Pradii03 (ringApply help route).
## The fits use the single-level random parts of helper-fits.R; the
## three-level listing (sample, tree, plot) is checked in
## test-default-route-3-levels.R.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

test_that("summary and print list only the grouping level", {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    dfm2 <- ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003,
                      fn = "scacum")
    fit <- fit_tree(dfm2)
    expect_equal(summary(fit)$groups, c(tree = 4L))
    out <- capture.output(print(fit))
    i <- which(out == "Groups:")
    expect_length(i, 1L)
    printed <- strsplit(trimws(out[i + 1L]), "\\s+")[[1]]
    expect_equal(printed, "tree")
    expect_false(any(c("X", "y") %in% printed))
})

test_that("modelFrame(Pchron) prints only the grouping level", {
    data(Pchron, package = "BIOdry", envir = environment())
    m <- modelFrame(Pchron, to = "cm",
                    MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                    form = "log(x) ~ log(csx) + time | tree",
                    random = nlme::pdDiag(~ log(csx)))
    expect_equal(summary(m)$groups, c(tree = 4L))
    out <- capture.output(print(m))
    i <- which(out == "Groups:")
    expect_length(i, 1L)
    printed <- strsplit(trimws(out[i + 1L]), "\\s+")[[1]]
    counts <- as.integer(strsplit(trimws(out[i + 2L]), "\\s+")[[1]])
    expect_equal(printed, "tree")
    expect_equal(counts, 4L)
})
