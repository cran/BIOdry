## Regression test: normalizeGroupLevels() orders a grouping column as
## numbers only when all its non-missing labels read as numbers, with the
## labels themselves as levels; a column that mixes number-like and other
## labels keeps every label. Labels such as "T7" or "07" were turned
## into NA.
## Bundled data only: Prings05 through ringApply(), with one tree label
## recoded to "T7" or "07" (user-approved recodes).
data(Prings05, package = "BIOdry", envir = environment())

test_that("a non-numeric tree label is kept, with no NA introduced", {
    d <- ringApply(Prings05, lv = 2, fn = "rtimes")
    tr <- as.character(d$tree)
    tr[tr == "7"] <- "T7"
    d$tree <- factor(tr)
    out <- normalizeGroupLevels(d, groups = "tree")
    expect_false(anyNA(out$tree))
    expect_identical(as.character(out$tree), tr)
    expect_setequal(levels(out$tree), c("17", "18", "56", "T7"))
    expect_identical(nrow(out), nrow(d))
})

test_that("all-numeric labels are still ordered as numbers", {
    d <- ringApply(Prings05, lv = 2, fn = "rtimes")
    out <- normalizeGroupLevels(d, groups = "tree")
    expect_identical(levels(out$tree), c("7", "17", "18", "56"))
    expect_identical(as.character(out$tree), as.character(d$tree))
})

test_that("a tree label '07' is kept and ordered before 17", {
    d <- ringApply(Prings05, lv = 2, fn = "rtimes")
    tr <- as.character(d$tree)
    tr[tr == "7"] <- "07"
    d$tree <- factor(tr)
    out <- normalizeGroupLevels(d, groups = "tree")
    expect_false(anyNA(out$tree))
    expect_identical(as.character(out$tree), tr)
    expect_identical(levels(out$tree), c("07", "17", "18", "56"))
})
