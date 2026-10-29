## Regression test: amod() must take the first increment of each core as the
## change implied by its own ring, f(csx1) - f(csx1 - x1), not as the whole
## transformed size f(csx1). This matters when csx is scaled (does not start
## at the first ring width), as on the ringApply help route.
## Bundled data only: Prings05, Pradii03.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

## ringApply help example: basal area from radius, f(r) = 0.25 * pi * (2 r)^2,
## written as two allometric steps mp = c(2, 1, 0.25 * pi, 2)
mp <- c(2, 1, 0.25 * pi, 2)
f <- function(r) 0.25 * pi * (2 * r)^2

route <- function(scaled = TRUE) {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    dfm2 <- if (scaled) {
        ringApply(dfm1, lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
    } else {
        ringApply(dfm1, lv = "sample", fn = "scacum")
    }
    dfm3 <- ringApply(dfm2, lv = "sample", fn = "amod",
                      MoreArgs = list(mp = mp))
    key <- function(d) paste(d$plot, d$tree, d$sample, d$year)
    a <- as.data.frame(dfm2)
    b <- as.data.frame(dfm3)
    list(input = a, output = b[match(key(a), key(b)), ])
}

test_that("every increment equals f(csx) - f(csx - x) on the scaled route", {
    r <- route(scaled = TRUE)
    expected <- f(r$input$csx) - f(r$input$csx - r$input$x)
    expect_equal(r$output$x, expected, tolerance = 1e-8)
    expect_equal(r$output$csx, f(r$input$csx), tolerance = 1e-8)
})

test_that("P16106 tree 17 core a, 1897, gets the increment of its own ring", {
    r <- route(scaled = TRUE)
    i <- with(r$input, which(plot == "P16106" & tree == "17" & sample == "a" &
                             year == 1897))
    expect_length(i, 1L)
    csx1 <- r$input$csx[i]
    x1 <- r$input$x[i]
    expect_equal(r$output$x[i], f(csx1) - f(csx1 - x1), tolerance = 1e-8)
    expect_lt(r$output$x[i], f(csx1) / 10)
})

test_that("an unscaled cumulative series keeps its first increment f(x1)", {
    r <- route(scaled = FALSE)
    expected <- f(r$input$csx) - f(r$input$csx - r$input$x)
    expect_equal(r$output$x, expected, tolerance = 1e-8)
})
