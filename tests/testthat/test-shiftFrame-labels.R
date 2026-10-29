## Regression test: shiftFrame() must read dot-separated wide column names
## from the highest level to the lowest, e.g. 'P16106.17.a' is core 'a' of
## tree '17' in plot 'P16106' (as in BIOdry 0.9.1).
## Bundled data only: Pchron, Temp, Prec.
data(Pchron, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())
data(Prec, package = "BIOdry", envir = environment())

## Expected long frame built from the wide data: one row per non-missing
## value, labels taken from the column name, highest level first.
expected_long <- function(wide, levels_high_to_low, value = "x") {
    do.call(rbind, lapply(names(wide), function(nm) {
        v <- wide[[nm]]
        ok <- !is.na(v)
        lab <- strsplit(nm, ".", fixed = TRUE)[[1L]]
        out <- data.frame(year = as.integer(rownames(wide))[ok], v = v[ok])
        for (k in seq_along(levels_high_to_low)) out[[levels_high_to_low[k]]] <- lab[k]
        names(out)[2L] <- value
        out
    }))
}

## Every expected row must be found under the same labels and year with the
## same value, and the long frame must have no other rows.
expect_same_labels <- function(long, exp, keys, value) {
    long <- as.data.frame(long)
    key <- function(d) do.call(paste, c(lapply(d[c(keys, "year")], as.character), sep = "|"))
    i <- match(key(exp), key(long))
    expect_false(anyNA(i))
    expect_equal(nrow(long), nrow(exp))
    expect_equal(as.numeric(long[[value]][i]), as.numeric(exp[[value]]))
}

test_that("shiftFrame(Pchron) gives plot = P16106, tree = 17, sample = a", {
    long <- shiftFrame(Pchron)
    exp <- expected_long(Pchron, c("plot", "tree", "sample"))
    expect_same_labels(long, exp, c("plot", "tree", "sample"), "x")
    d <- as.data.frame(long)
    expect_setequal(as.character(unique(d$plot)), c("P16106", "P44005"))
    expect_setequal(as.character(unique(d$sample)), c("a", "b"))
    trees <- tapply(as.character(d$tree), as.character(d$plot),
                    function(z) sort(unique(z)), simplify = FALSE)
    expect_equal(trees[["P16106"]], c("17", "18"))
    expect_equal(trees[["P44005"]], c("56", "7"))
})

test_that("shiftFrame on climate data gives plot = P16106, month = Apr", {
    for (nm in c("Temp", "Prec")) {
        wide <- get(nm)
        value <- attr(wide, "x.nm")
        long <- shiftFrame(wide)
        exp <- expected_long(wide, c("plot", "month"), value = value)
        expect_same_labels(long, exp, c("plot", "month"), value)
        d <- as.data.frame(long)
        expect_setequal(as.character(unique(d$plot)), c("P16106", "P44005"))
        expect_setequal(as.character(unique(d$month)), month.abb)
    }
})

test_that("modelFrame(Pchron) fits plot/tree/sample on the right labels", {
    fit <- modelFrame(Pchron)
    md <- as.data.frame(nlme::getData(fit$model))
    expect_setequal(as.character(unique(md$plot)), c("P16106", "P44005"))
    expect_true(all(tapply(as.character(md$tree), as.character(md$plot),
                           function(z) length(unique(z))) == 2L))
    ## 0.9.1 reference: 2 plots, 4 trees, 8 cores
    ng <- fit$model$dims$ngrps
    expect_equal(unname(ng[c("plot", "tree", "sample")]), c(2L, 4L, 8L))
})
