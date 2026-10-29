## Regression test: frametoLme() must build $fluc for models with a single
## grouping level, where nlme returns the residuals as a vector named by
## group instead of a matrix with row labels.
## Bundled data only: Prec, Temp, following the muleMan help example
## (aridity-index fluctuations with moveYr and wlai through modelFrame).
data(Prec, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())

aridity_fluc <- function() {
    modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
               lv = list("year", "year"), form = "lmeForm")
}

test_that("$fluc of a one-level model equals its Pearson residuals", {
    aif <- aridity_fluc()
    m <- aif$model
    expect_equal(ncol(m$groups), 1L)
    expect_false(is.null(aif$fluc))
    md <- as.data.frame(m$data)
    fl <- as.data.frame(aif$fluc)
    expect_equal(nrow(fl), nrow(md))
    lv <- names(m$groups)
    tv <- all.vars(nlme::getCovariateFormula(aif$fluc))
    key <- function(d) paste(d[[lv]], d[[tv]])
    i <- match(key(fl), key(md))
    expect_false(anyNA(i))
    ## with level 0 added, nlme returns a matrix whose row labels are the
    ## model-data row labels (a single level alone is named by group)
    pr <- residuals(m, level = c(1, 0), type = "pearson")
    expect_identical(rownames(pr), rownames(md))
    expect_equal(fl[[paste0(lv, ".res")]], unname(pr[i, lv]), tolerance = 1e-10)
})
