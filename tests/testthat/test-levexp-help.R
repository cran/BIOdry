## Regression test: the levexp() help call levexp(Pradii03, Prings05) must
## give every core the radius of its own tree. It returned an empty vector:
## the integer 'year' column was taken as a grouping column, and a name with
## two levels such as "P16106.17" could not match a core label.
## Bundled data only: Pradii03, Prings05.
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

test_that("levexp(Pradii03, Prings05) gives every core its own tree's radius", {
    r <- levexp(Pradii03, Prings05)
    cores <- unique(Prings05[, c("plot", "tree", "sample")])
    expect_length(r, nrow(cores))
    tokens <- strsplit(names(r), ".", fixed = TRUE)
    for (k in seq_len(nrow(cores))) {
        core <- as.character(unlist(cores[k, ]))
        i <- which(vapply(tokens, function(t) setequal(t, core), logical(1)))
        expect_length(i, 1L)
        expect_equal(unname(r[i]),
                     unname(Pradii03[paste(core[1L], core[2L], sep = ".")]))
    }
})
