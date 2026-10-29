## Regression test: normalizeGroupLevels() on a frametoLme fit, without
## 'groups', orders the grouping levels of $fluc as numbers. The fit
## carries no grouping metadata itself, so the grouping variables come
## from $fluc's metadata, else from the model's grouping formula.
## Bundled data only: Prings05, Pradii03 (frametoLme help route) and
## Pchron (modelFrame help route).
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())
data(Pchron, package = "BIOdry", envir = environment())

example_fit <- function() {
    Prings05 |>
        ringApply(lv = 2, fn = "rtimes") |>
        ringApply(lv = "sample", sc.c = Pradii03, rf.t = 2003,
                  fn = "scacum") |>
        frametoLme(form = "log(x) ~ log(csx) + log(time) | tree",
                   random = pdDiag(~ log(csx)))
}

test_that("frametoLme example fit: tree levels become 7, 17, 18, 56", {
    fit <- example_fit()
    expect_identical(levels(fit$fluc$tree), c("18", "7", "56", "17"))
    out <- normalizeGroupLevels(fit)
    expect_identical(levels(out$fluc$tree), c("7", "17", "18", "56"))
    ## same as giving the grouping variable
    expect_identical(levels(normalizeGroupLevels(fit, groups = "tree")$fluc$tree),
                     levels(out$fluc$tree))
    ## values and rows are unchanged
    expect_identical(as.character(out$fluc$tree), as.character(fit$fluc$tree))
    expect_identical(out$fluc$tree.res, fit$fluc$tree.res)
    expect_identical(out$model, fit$model)
})

test_that("modelFrame biomass fit: tree levels become 7, 17, 18, 56", {
    mf <- suppressWarnings(
        modelFrame(Pchron, to = "cm",
                   MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                   log.t = FALSE, on.time = FALSE))
    out <- normalizeGroupLevels(mf)
    expect_identical(levels(out$fluc$tree), c("7", "17", "18", "56"))
    expect_identical(as.character(out$fluc$tree), as.character(mf$fluc$tree))
})

test_that("without $fluc metadata the model's grouping formula is used", {
    fit <- example_fit()
    attr(fit$fluc, "BIOdry_meta") <- NULL
    out <- normalizeGroupLevels(fit)
    expect_identical(levels(out$fluc$tree), c("7", "17", "18", "56"))
})
