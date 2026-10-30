## Regression test: frametoLme() must keep each $fluc residual on its own row.
## Bundled data only: Prings05 (long, one row per ring), Pradii03, Pchron.
## The fits use the single-level random parts of helper-fits.R; the
## three-level default route is checked in test-default-route-3-levels.R.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())
data(Pchron, package = "BIOdry", envir = environment())

## $fluc must equal the Pearson residuals of the fitted model row by row,
## matched by row name; its time and grouping columns must be those of the
## input row with the same name; and fitted value plus residual must give
## back log(x) of that input row.
expect_fluc_is_pearson <- function(fit, input) {
    md <- as.data.frame(fit$model$data)
    fl <- as.data.frame(fit$fluc)
    expect_equal(nrow(fl), nrow(md))
    i <- match(rownames(fl), rownames(md))
    expect_false(anyNA(i))
    for (v in setdiff(names(fl), "tree.res"))
        expect_equal(as.character(fl[[v]]),
                     as.character(input[rownames(fl), v]), info = v)
    pr <- residuals(fit$model, type = "pearson")
    expect_equal(fl$tree.res, unname(pr[i]), tolerance = 1e-10)
    rebuilt <- fitted(fit$model)[i] + fl$tree.res * fit$model$sigma
    expect_lt(max(abs(unname(rebuilt) - log(input[rownames(fl), "x"]))), 1e-8)
}

## Shuffled or sorted rows hold the same data, so the fit must not change.
expect_same_fit <- function(fit, ref) {
    expect_equal(as.numeric(logLik(fit$model)), as.numeric(logLik(ref$model)),
                 tolerance = 1e-6)
    expect_equal(nlme::fixef(fit$model), nlme::fixef(ref$model), tolerance = 1e-6)
}

not_1_to_n <- function(d) !identical(rownames(d), as.character(seq_len(nrow(d))))

## ringApply help example: tree-level rtimes, then sample-level scacum
## around the radii measured in 2003.
rings_rtimes_scacum <- function() {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

test_that("$fluc is aligned on the documented routes", {
    rd <- ringApply(defineDataRoles(Prings05, response = "x", time = "year",
                                    groups = "plot/tree/sample"),
                    lv = "sample", fn = "scacum")
    expect_fluc_is_pearson(fit_tree(rd, "year"), rd)
    rd2 <- rings_rtimes_scacum()
    expect_fluc_is_pearson(fit_tree(rd2), rd2)
    pc <- modelFrame(Pchron, form = NULL)
    expect_fluc_is_pearson(
        modelFrame(Pchron, form = "log(x) ~ log(csx) + log(time) | tree",
                   random = nlme::pdDiag(~ log(csx))), pc)
})

test_that("$fluc stays aligned, and the fit unchanged, when rows are shuffled", {
    rd <- rings_rtimes_scacum()
    ref <- fit_tree(rd)
    set.seed(1)
    shuffled <- rd[sample(nrow(rd)), ]
    expect_true(not_1_to_n(shuffled))
    fit <- fit_tree(shuffled)
    expect_fluc_is_pearson(fit, shuffled)
    expect_same_fit(fit, ref)
})

test_that("$fluc stays aligned, and the fit unchanged, when rows are sorted", {
    rd <- rings_rtimes_scacum()
    ref <- fit_tree(rd)
    by_year <- rd[order(rd$year, decreasing = TRUE), ]
    by_sample <- rd[order(rd$sample, rd$year), ]
    expect_true(not_1_to_n(by_year))
    expect_true(not_1_to_n(by_sample))
    fit_year <- fit_tree(by_year)
    fit_sample <- fit_tree(by_sample)
    expect_fluc_is_pearson(fit_year, by_year)
    expect_fluc_is_pearson(fit_sample, by_sample)
    expect_same_fit(fit_year, ref)
    expect_same_fit(fit_sample, ref)
})

test_that("$fluc stays aligned when rows are dropped after rtimes and scacum", {
    rd <- rings_rtimes_scacum()
    dropped <- rd[-c(5, 50, 500), ]
    expect_true(not_1_to_n(dropped))
    expect_fluc_is_pearson(fit_tree(dropped), dropped)
    set.seed(1)
    shuffled_dropped <- rd[sample(nrow(rd)), ][-c(5, 50, 500), ]
    expect_fluc_is_pearson(fit_tree(shuffled_dropped), shuffled_dropped)
})
