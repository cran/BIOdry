## Regression test: when the model uses a time variable other than the first
## one detected in the input (here the rtimes column 'time' next to 'year'),
## frametoLme() must build $fluc on the time variable the model used.
## Bundled data only: Prings05, Pradii03.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

## ringApply help example: tree-level rtimes, then sample-level scacum
## around the radii measured in 2003; the result has both 'year' and 'time'.
rings_rtimes_scacum <- function() {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
}

expect_fluc_on_time <- function(fit) {
    expect_equal(deparse(formula(fit$model)), "log(x) ~ log(csx) + time")
    expect_false(is.null(fit$fluc))
    expect_equal(deparse(nlme::getCovariateFormula(fit$fluc)), "~time")
    md <- as.data.frame(fit$model$data)
    fl <- as.data.frame(fit$fluc)
    expect_equal(nrow(fl), nrow(md))
    key <- function(d) paste(d$plot, d$tree, d$sample, d$time)
    i <- match(key(fl), key(md))
    expect_false(anyNA(i))
    pr <- residuals(fit$model, level = 1:3, type = "pearson")
    lv <- c("plot", "tree", "sample")
    expect_equal(unname(as.matrix(fl[, paste0(lv, ".res")])),
                 unname(pr[i, lv]), tolerance = 1e-10)
}

test_that("$fluc uses 'time' when it is declared with defineDataRoles", {
    rd <- defineDataRoles(rings_rtimes_scacum(), response = "x", time = "time",
                          groups = "plot/tree/sample")
    expect_fluc_on_time(frametoLme(rd, form = "tdForm"))
})

test_that("$fluc uses 'time' when the formula string uses it", {
    fit <- frametoLme(rings_rtimes_scacum(),
                      form = "log(x) ~ log(csx) + time | plot/tree/sample")
    expect_fluc_on_time(fit)
})
