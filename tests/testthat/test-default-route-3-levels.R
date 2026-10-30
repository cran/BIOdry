## Checks of the documented three-level route (plot/tree/sample with the
## default random part). On the bundled data (2 plots x 2 trees x 2 cores)
## this fit is singular or close to it, and its estimates differ between
## platforms, so these tests do not run on CRAN and never compare fitted
## values: they check only that the outputs agree with the fit they come
## from (alignment and labels). The same properties are checked on CRAN with
## the single-level fits of helper-fits.R.
## Bundled data only: Prings05, Pradii03, Pchron, Prec, Temp.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())
data(Pchron, package = "BIOdry", envir = environment())
data(Prec, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())

rings_rtimes_scacum <- function(rd = Prings05) {
    dfm1 <- ringApply(rd, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

## $fluc must equal the Pearson residuals of the fitted model at every
## grouping level, row by row, matched on plot/tree/sample and the time
## variable of $fluc, and fitted plus residual must give back log(x).
expect_fluc_is_pearson_3 <- function(fit) {
    md <- as.data.frame(fit$model$data)
    fl <- as.data.frame(fit$fluc)
    tv <- all.vars(nlme::getCovariateFormula(fit$fluc))
    expect_length(tv, 1L)
    key <- function(d) paste(d$plot, d$tree, d$sample, d[[tv]])
    i <- match(key(fl), key(md))
    expect_false(anyNA(i))
    expect_equal(nrow(fl), nrow(md))
    pr <- residuals(fit$model, level = 1:3, type = "pearson")
    lv <- c("plot", "tree", "sample")
    expect_equal(unname(as.matrix(fl[, paste0(lv, ".res")])),
                 unname(pr[i, lv]), tolerance = 1e-10)
    rebuilt <- fitted(fit$model, level = 3)[i] + fl$sample.res * fit$model$sigma
    expect_lt(max(abs(as.numeric(rebuilt) - log(md$x[i]))), 1e-8)
}

test_that("3 levels: $fluc is aligned on the documented routes", {
    skip_on_cran()
    rd <- ringApply(defineDataRoles(Prings05, response = "x", time = "year",
                                    groups = "plot/tree/sample"),
                    lv = "sample", fn = "scacum")
    expect_fluc_is_pearson_3(frametoLme(rd, form = "tdForm"))
    expect_fluc_is_pearson_3(frametoLme(rings_rtimes_scacum(), form = "tdForm"))
    expect_fluc_is_pearson_3(modelFrame(Pchron))
})

test_that("3 levels: $fluc stays aligned when rows are shuffled, sorted or dropped", {
    skip_on_cran()
    rd <- rings_rtimes_scacum()
    set.seed(1)
    expect_fluc_is_pearson_3(frametoLme(rd[sample(nrow(rd)), ], form = "tdForm"))
    expect_fluc_is_pearson_3(
        frametoLme(rd[order(rd$year, decreasing = TRUE), ], form = "tdForm"))
    expect_fluc_is_pearson_3(frametoLme(rd[-c(5, 50, 500), ], form = "tdForm"))
})

test_that("3 levels: $fluc uses a declared 'time'", {
    skip_on_cran()
    rd <- defineDataRoles(rings_rtimes_scacum(), response = "x", time = "time",
                          groups = "plot/tree/sample")
    fit <- frametoLme(rd, form = "tdForm")
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + time")
    expect_equal(deparse(nlme::getCovariateFormula(fit$fluc)), "~time")
    expect_fluc_is_pearson_3(fit)
})

test_that("3 levels: frametoLme fits the formula tdForm gives", {
    skip_on_cran()
    fitted_form <- function(rd, ...)
        deparse(formula(frametoLme(rd, form = "tdForm", ...)$model))
    tdform_rhs <- function(rd, ...)
        sub(" \\|.*$", "", deparse(tdForm(rd, ...)))
    rd <- rings_rtimes_scacum()
    expect_equal(fitted_form(rd), tdform_rhs(rd))
    expect_equal(fitted_form(rd, on.time = FALSE), tdform_rhs(rd, on.time = FALSE))
    dy <- rings_rtimes_scacum(defineDataRoles(Prings05, response = "x",
                                              time = "year",
                                              groups = "plot/tree/sample"))
    expect_equal(fitted_form(dy), tdform_rhs(dy))
    expect_equal(fitted_form(dy), "log(x) ~ log(csx) + year")
})

test_that("3 levels: modelFrame with lmeForm fits the formula lmeForm gives", {
    skip_on_cran()
    ai <- modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
                     lv = list("year", "year"), form = NULL)
    m <- modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
                    lv = list("year", "year"), form = "lmeForm")
    expect_equal(deparse(formula(m$model)),
                 sub(" \\|.*$", "", deparse(lmeForm(ai))))
    expect_equal(m$model$dims$N, 100L)
})

## Refitting a fit's model data must give the same nesting as the original
## fit. The slim fitting copy put the grouping columns after the formula
## variables (x csx time plot tree sample), and tdForm, which takes the outer
## level first as the reverse of the column order, then built
## sample/tree/plot.
test_that("3 levels: refitting fit$model$data with tdForm keeps plot/tree/sample", {
    skip_on_cran()
    dfm2 <- rings_rtimes_scacum()
    fit <- frametoLme(dfm2, form = "tdForm")
    expect_equal(names(fit$model$groups), c("plot", "tree", "sample"))
    expect_equal(deparse(tdForm(fit$model$data, on.time = FALSE)),
                 "log(x) ~ log(csx) + year | plot/tree/sample")
    refit <- frametoLme(fit$model$data, form = "tdForm", on.time = FALSE)
    expect_equal(names(refit$model$groups), c("plot", "tree", "sample"))
    ## the grouping columns of the slim copy keep the input's order
    g <- c("plot", "tree", "sample")
    expect_equal(intersect(names(fit$model$data), g),
                 intersect(names(dfm2), g))
})

## The summary of a frametoLme fit must list only the model's grouping
## levels, not nlme's model matrix and response ("X 1, y 1").
test_that("3 levels: summary and print list only sample, tree and plot", {
    skip_on_cran()
    fit <- frametoLme(rings_rtimes_scacum(), form = "tdForm")
    expect_equal(summary(fit)$groups, c(sample = 8L, tree = 4L, plot = 2L))
    m <- modelFrame(Pchron, to = "cm",
                    MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                    log.t = FALSE)
    out <- capture.output(print(m))
    i <- which(out == "Groups:")
    expect_length(i, 1L)
    expect_equal(strsplit(trimws(out[i + 1L]), "\\s+")[[1]],
                 c("sample", "tree", "plot"))
    expect_equal(as.integer(strsplit(trimws(out[i + 2L]), "\\s+")[[1]]),
                 c(8L, 4L, 2L))
})

## shiftFrame labels: modelFrame(Pchron) fits plot/tree/sample on 2 plots,
## 4 trees and 8 cores, as 0.9.1 did.
test_that("3 levels: modelFrame(Pchron) fits plot/tree/sample on the right labels", {
    skip_on_cran()
    fit <- modelFrame(Pchron)
    expect_equal(unname(fit$model$dims$ngrps[c("plot", "tree", "sample")]),
                 c(2L, 4L, 8L))
})

## normalizeGroupLevels on the modelFrame biomass fit, whose $fluc lists the
## trees as text (17, 18, 56, 7)
test_that("3 levels: normalizeGroupLevels orders the modelFrame biomass trees", {
    skip_on_cran()
    mf <- suppressWarnings(
        modelFrame(Pchron, to = "cm",
                   MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                   log.t = FALSE, on.time = FALSE))
    out <- normalizeGroupLevels(mf)
    expect_identical(levels(out$fluc$tree), c("7", "17", "18", "56"))
    expect_identical(as.character(out$fluc$tree), as.character(mf$fluc$tree))
})

## plot() of a fit without $fluc rebuilds the same residual points; one
## fit is plotted with and without its $fluc, so no two fits are compared
test_that("3 levels: plot() without $fluc shows the same points", {
    skip_on_cran()
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    fit <- frametoLme(rings_rtimes_scacum(), form = "tdForm")
    nofluc <- fit
    nofluc$fluc <- NULL
    pts <- function(tr) lapply(tr$panel.args, function(p) {
        o <- order(p$x, p$y); cbind(as.numeric(p$x[o]), as.numeric(p$y[o])) })
    a <- plot(fit, print_trellis = FALSE)
    b <- plot(nofluc, print_trellis = FALSE)
    expect_s3_class(b, "trellis")
    expect_equal(pts(b), pts(a), tolerance = 1e-10)
})
