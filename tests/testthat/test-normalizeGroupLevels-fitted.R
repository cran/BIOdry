## Regression test: normalizeGroupLevels() on a frametoLme fit, without
## 'groups', orders the grouping levels of $fluc as numbers. The fit
## carries no grouping metadata itself, so the grouping variables come
## from $fluc's metadata, else from the model's grouping formula.
## Bundled data only: Prings05, Pradii03 (frametoLme help route). The
## modelFrame biomass route (three levels) is checked in
## test-default-route-3-levels.R.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

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

test_that("without $fluc metadata the model's grouping formula is used", {
    fit <- example_fit()
    attr(fit$fluc, "BIOdry_meta") <- NULL
    out <- normalizeGroupLevels(fit)
    expect_identical(levels(out$fluc$tree), c("7", "17", "18", "56"))
})
