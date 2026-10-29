## Regression tests for the Friedman status print and series order. The
## status table carried a long note column that wrapped at width 80 into a
## separate, mostly empty block; the notes are now listed below the table,
## one line per series with its name and number of NA values.
## frametoFriedman() listed the series in factor-level order (a/7/P44005
## first on the ringApply help route); it now keeps the order of the
## series' first rows in the data, as the modelFrame path does.
## Bundled data only: Pchron (modelFrame example), Prings05 and Pradii03
## (ringApply help route).
data(Pchron, package = "BIOdry", envir = environment())
data(Prings05, package = "BIOdry", envir = environment())
data(Pradii03, package = "BIOdry", envir = environment())

test_that("notes are listed below the status table with their NA counts", {
    f <- Pchron |>
        modelFrame(form = "FriedmanForm", to = "cm",
                   MoreArgs = list(mp = c(2, 1, 2.87, 0.85, 0.05, 2.5)),
                   response = "x", time = "year")
    old <- options(width = 80)
    on.exit(options(old), add = TRUE)
    out <- capture.output(print(f))
    i <- which(out == "Series status (first rows):")
    j <- which(out == "Notes:")
    expect_length(i, 1L)
    expect_length(j, 1L)
    if (length(i) != 1L || length(j) != 1L) return(invisible())
    ## the table: header plus 8 series, no note column, no empty lines
    tab <- out[(i + 1L):(j - 2L)]
    expect_length(tab, 9L)
    expect_false(grepl("note", tab[1L]))
    expect_false(any(grepl("^\\s*$", tab)))
    ## one line per series with a note, naming it and its NA count, within
    ## 80 columns; the full note text stays in the series table
    notes <- out[(j + 1L):(j + 2L)]
    expect_equal(trimws(notes[1L]), "a/17/P16106: 1 NA ratio index (non-positive fitted values)")
    expect_equal(trimws(notes[2L]), "b/18/P16106: 1 NA ratio index (non-positive fitted values)")
    expect_true(all(nchar(out) <= 80L))
    s <- f$model$series
    expect_equal(s$note[s$sample == "a" & s$tree == "17"],
                 "non-positive fitted values produced NA ratio detrending")
    expect_equal(sum(s$n_na), 2L)
    ## the NA counts agree with $fluc
    fl <- as.data.frame(f$fluc)
    expect_equal(sum(is.na(fl$x.idx)), 2L)
})

test_that("frametoFriedman keeps the data order of the series and prints no notes", {
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = "rtimes") |>
        ringApply(lv = "sample", sc.c = Pradii03, rf.t = 2003, fn = "scacum")
    ff <- frametoFriedman(dfm2)
    s <- ff$model$series
    expect_equal(paste(s$sample, s$tree, s$plot),
                 unique(paste(dfm2$sample, dfm2$tree, dfm2$plot)))
    out <- capture.output(print(ff))
    expect_false(any(out == "Notes:"))
})
