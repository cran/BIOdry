## Regression test: the residual plots of fitted models must fill their
## panels and label their axes. nlme's plot.groupedData banks to 45 degrees
## by default (aspect = "xy"), which collapsed plot.modelFrame's residual
## panels to thin strips (aspect ratio 0.016), and the axes carried internal
## column names (.mf_resid__, .mf_fitted__, .model_resid__). Options given
## by the user (aspect, xlab, ylab) must still be respected.
## Bundled data only: Pchron (modelFrame example), Prings05 and Pradii03
## (ringApply help route). The fits use the single-level random parts of
## helper-fits.R; on the bundled data the default three-level fit is near
## singular.
data(Pchron, package = "BIOdry", envir = environment())
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

lab <- function(x) as.character(unlist(x))[1L]

test_that("plot.modelFrame of a fitted model fills the panels and labels the axes", {
    m <- Pchron |>
        modelFrame(to = "cm", MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                   form = "log(x) ~ log(csx) + year | tree",
                   random = nlme::pdDiag(~ 1))
    tr <- plot(m, print.trellis = FALSE)
    expect_true(tr$aspect.fill)
    expect_equal(lab(tr$ylab), "Pearson residuals")
    expect_equal(lab(tr$xlab), "Fitted values")
    own <- plot(m, aspect = "xy", xlab = "fitted", ylab = "residuals",
                print.trellis = FALSE)
    expect_false(own$aspect.fill)
    expect_equal(lab(own$xlab), "fitted")
    expect_equal(lab(own$ylab), "residuals")
})

test_that("plot.frametoLme fills the panels and labels the residual axis", {
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = "rtimes") |>
        ringApply(lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
    for (rd in c(TRUE, FALSE)) {
        fit <- fit_tree(dfm2, "time", res.data = rd)
        tr <- plot(fit, print.trellis = FALSE)
        expect_true(tr$aspect.fill)
        expect_equal(lab(tr$ylab), "Pearson residuals")
        expect_equal(lab(tr$xlab), "time")
    }
    own <- plot(fit_tree(dfm2, "time"), aspect = "xy",
                ylab = "residuals", print.trellis = FALSE)
    expect_false(own$aspect.fill)
    expect_equal(lab(own$ylab), "residuals")
})

test_that("plot.modelFrame of processed data (form = NULL) fills the panels", {
    r <- Pchron |>
        modelFrame(form = NULL, to = "cm",
                   MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)))
    tr <- plot(r, print.trellis = FALSE)
    expect_true(tr$aspect.fill)
    own <- plot(r, aspect = "xy", print.trellis = FALSE)
    expect_false(own$aspect.fill)
})
