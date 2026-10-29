## Regression test: frametoLme() must keep each $fluc residual on its own row.
## Bundled data only: Prings05 (long, one row per ring), Pradii03, Pchron.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())
data(Pchron, package = "BIOdry", envir = environment())

## rows are matched on plot/tree/sample and the time variable that $fluc was
## built on (the fitted model's time variable: 'year' or the rtimes 'time')
key <- function(d, tv) paste(d$plot, d$tree, d$sample, d[[tv]])

## $fluc must equal the Pearson residuals of the fitted model at every
## grouping level, row by row, matched through the plot/tree/sample/time key
## (see key() above), and fitted plus residual must give back log(x) on every
## row.
expect_fluc_is_pearson <- function(fit) {
    md <- as.data.frame(fit$model$data)
    fl <- as.data.frame(fit$fluc)
    tv <- all.vars(nlme::getCovariateFormula(fit$fluc))
    expect_length(tv, 1L)
    i <- match(key(fl, tv), key(md, tv))
    expect_false(anyNA(i))
    expect_equal(nrow(fl), nrow(md))
    pr <- residuals(fit$model, level = 1:3, type = "pearson")
    lv <- c("plot", "tree", "sample")
    expect_equal(unname(as.matrix(fl[, paste0(lv, ".res")])),
                 unname(pr[i, lv]), tolerance = 1e-10)
    rebuilt <- fitted(fit$model, level = 3)[i] + fl$sample.res * fit$model$sigma
    expect_lt(max(abs(as.numeric(rebuilt) - log(md$x[i]))), 1e-8)
}

## Shuffled or sorted rows hold the same data, so the fit must not change.
expect_same_fit <- function(fit, ref) {
    expect_equal(as.numeric(logLik(fit$model)), as.numeric(logLik(ref$model)),
                 tolerance = 1e-8)
    expect_equal(nlme::fixef(fit$model), nlme::fixef(ref$model), tolerance = 1e-8)
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
    expect_fluc_is_pearson(frametoLme(rd, form = "tdForm"))
    expect_fluc_is_pearson(frametoLme(rings_rtimes_scacum(), form = "tdForm"))
    expect_fluc_is_pearson(modelFrame(Pchron))
})

test_that("$fluc stays aligned, and the fit unchanged, when rows are shuffled", {
    rd <- rings_rtimes_scacum()
    ref <- frametoLme(rd, form = "tdForm")
    set.seed(1)
    shuffled <- rd[sample(nrow(rd)), ]
    expect_true(not_1_to_n(shuffled))
    fit <- frametoLme(shuffled, form = "tdForm")
    expect_fluc_is_pearson(fit)
    expect_same_fit(fit, ref)
})

test_that("$fluc stays aligned, and the fit unchanged, when rows are sorted", {
    rd <- rings_rtimes_scacum()
    ref <- frametoLme(rd, form = "tdForm")
    by_year <- rd[order(rd$year, decreasing = TRUE), ]
    by_sample <- rd[order(rd$sample, rd$year), ]
    expect_true(not_1_to_n(by_year))
    expect_true(not_1_to_n(by_sample))
    fit_year <- frametoLme(by_year, form = "tdForm")
    fit_sample <- frametoLme(by_sample, form = "tdForm")
    expect_fluc_is_pearson(fit_year)
    expect_fluc_is_pearson(fit_sample)
    expect_same_fit(fit_year, ref)
    expect_same_fit(fit_sample, ref)
})

test_that("$fluc stays aligned when rows are dropped after rtimes and scacum", {
    rd <- rings_rtimes_scacum()
    dropped <- rd[-c(5, 50, 500), ]
    expect_true(not_1_to_n(dropped))
    expect_fluc_is_pearson(frametoLme(dropped, form = "tdForm"))
    set.seed(1)
    shuffled_dropped <- rd[sample(nrow(rd)), ][-c(5, 50, 500), ]
    expect_fluc_is_pearson(frametoLme(shuffled_dropped, form = "tdForm"))
})
