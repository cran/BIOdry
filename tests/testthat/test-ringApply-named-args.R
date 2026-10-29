## Regression test: ringApply() must give a named argument such as sc.c to
## each block by name, not by position. With the elements of Pradii03 in
## reverse order (every radius keeps its name), each core must still be
## scaled to its own tree's radius in 2003.
## Bundled data only: Prings05, Pradii03; rev() reorders whole elements.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

csx_2003 <- function(radii) {
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    d <- as.data.frame(ringApply(dfm1, lv = "sample", sc.c = radii,
                                 rf.t = 2003, fn = "scacum"))
    d <- d[d$year == 2003, c("plot", "tree", "sample", "csx")]
    d$own <- unname(Pradii03[paste(d$plot, d$tree, sep = ".")])
    d
}

test_that("sc.c with reversed elements gives each core its own radius", {
    d <- csx_2003(rev(Pradii03))
    expect_equal(nrow(d), 8L)
    expect_equal(d$csx, d$own, tolerance = 1e-8)
})

test_that("sc.c in its bundled order gives the same result", {
    a <- csx_2003(Pradii03)
    b <- csx_2003(rev(Pradii03))
    expect_equal(a$csx, b$csx, tolerance = 1e-10)
})

test_that("sc.c whose names do not match every block gives a warning", {
    radii <- Pradii03
    names(radii)[names(radii) == "P16106.17"] <- "P16106.99"
    expect_identical(unname(radii), unname(Pradii03))
    dfm1 <- ringApply(Prings05, lv = 2, fn = "rtimes")
    w <- character(0)
    withCallingHandlers(
        ringApply(dfm1, lv = "sample", sc.c = radii, rf.t = 2003, fn = "scacum"),
        warning = function(cnd) {
            w <<- c(w, conditionMessage(cnd))
            invokeRestart("muffleWarning")
        })
    hit <- grep("'sc.c'", w, value = TRUE, fixed = TRUE)
    expect_length(hit, 1L)
    expect_match(hit, "a.17.P16106, b.17.P16106", fixed = TRUE)
})

test_that("an argument passed to ... without a name works as before", {
    ## mUnits help route on Prings05: from = "mm", to = "cm"; the
    ## positional form fills ringApply's own arguments with their defaults
    named <- ringApply(Prings05, lv = "sample", fn = "mUnits",
                       from = "mm", to = "cm")
    one <- ringApply(Prings05, "sample", "mUnits", NULL, NULL, "auto", "none",
                     "mm", to = "cm")
    all <- ringApply(Prings05, "sample", "mUnits", NULL, NULL, "auto", "none",
                     "mm", "cm")
    expect_equal(sum(named$x), 72.0233, tolerance = 1e-10)
    expect_equal(one$x, named$x)
    expect_equal(all$x, named$x)
})
