## Regression test: the Friedman plots must fill their panels and label
## their axes. nlme's plot.groupedData banks panels to 45 degrees by default
## (aspect = "xy"), which shrank them (aspect ratios 0.065 and 0.105), and
## the axes carried column names (x.idx, x.fit). Each plot keeps its
## documented content: index against the fitted smooth on the modelFrame
## path, index against time for frametoFriedman objects. Options given by
## the user (aspect, xlab, ylab) must be respected.
## Bundled data only: Pchron (modelFrame example), Prings05 and Pradii03
## (ringApply help route).
data(Pchron, package = "BIOdry", envir = environment())
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

lab <- function(x) as.character(unlist(x))[1L]

test_that("plot() of a modelFrame Friedman fit fills the panels and labels the axes", {
    f <- Pchron |>
        modelFrame(form = "FriedmanForm", to = "cm",
                   MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                   response = "x", time = "year")
    tr <- plot(f, print.trellis = FALSE)
    expect_true(tr$aspect.fill)
    expect_equal(lab(tr$ylab), "Ratio index")
    expect_equal(lab(tr$xlab), "Fitted smooth")
    own <- plot(f, aspect = "xy", xlab = "smooth", ylab = "index",
                print.trellis = FALSE)
    expect_false(own$aspect.fill)
    expect_equal(lab(own$xlab), "smooth")
    expect_equal(lab(own$ylab), "index")
})

test_that("plot() of a frametoFriedman fit fills the panels and labels the axes", {
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = "rtimes") |>
        ringApply(lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
    ratio <- frametoFriedman(dfm2)
    tr <- plot(ratio, print.trellis = FALSE)
    expect_true(tr$aspect.fill)
    expect_equal(lab(tr$ylab), "Ratio index")
    expect_equal(lab(tr$xlab), "year")
    diff <- frametoFriedman(dfm2, difference = TRUE)
    expect_equal(lab(plot(diff, print.trellis = FALSE)$ylab), "Difference index")
    own <- plot(ratio, aspect = "xy", xlab = "calendar year", ylab = "index",
                print.trellis = FALSE)
    expect_false(own$aspect.fill)
    expect_equal(lab(own$xlab), "calendar year")
    expect_equal(lab(own$ylab), "index")
})
