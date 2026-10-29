## Regression test: wlai() on long calendar data must not depend on the row
## order of 'cd'. Without ini.mnt, the start month of each 12-month block was
## the month of the first row of 'cd', so shuffling whole rows of PTclim05
## changed every aridity index.
## Bundled data only: PTclim05; whole rows are shuffled, reversed or sorted
## with base R, no values or columns are changed.
data(PTclim05, package = "BIOdry", envir = environment())

ai_by_block <- function(cd) {
    a <- as.data.frame(wlai(cd))
    a <- a[order(as.character(a$plot), a$year), c("plot", "year", "AI")]
    rownames(a) <- NULL
    a$plot <- as.character(a$plot)
    a
}

test_that("shuffling whole PTclim05 rows gives identical aridity indexes", {
    ref <- ai_by_block(PTclim05)
    expect_equal(nrow(ref), 102L)
    for (seed in 1:3) {
        set.seed(seed)
        shuffled <- PTclim05[sample(nrow(PTclim05)), ]
        expect_equal(ai_by_block(shuffled), ref, tolerance = 1e-12)
    }
    expect_equal(ai_by_block(PTclim05[rev(seq_len(nrow(PTclim05))), ]), ref,
                 tolerance = 1e-12)
})
