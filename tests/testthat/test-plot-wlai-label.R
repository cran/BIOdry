## Regression test: the block label printed by plot.wlai() matches the
## year column of the selected block, under label_year = "end" (default)
## and "start". It showed hydro_year_id (the start year) in both cases.
## Bundled data only: PTclim05 through moveYr() and wlai().
data(PTclim05, package = "BIOdry", envir = environment())

ai_label <- function(ai, ...) {
    pdf(NULL)
    on.exit(dev.off())
    dev.control("enable")
    plot(ai, ...)
    dl <- recordPlot()[[1]]
    unname(unlist(lapply(dl, function(e)
        unlist(Filter(function(z) is.character(z) && any(grepl("^AI = ", z)),
                      e[[2]])))))
}

test_that("label_year = 'end': year 2001 is labelled P44005.2001", {
    ai <- subset(PTclim05, plot == "P44005") |> moveYr() |> wlai()
    row <- subset(as.data.frame(ai), year == 2001)
    expect_identical(row$hydro_year_id, 2000L)
    lab <- ai_label(ai, year = 2001)
    expect_identical(lab, sprintf("AI = %s  [P44005.2001]", round(row$AI, 3)))
    ## selected by hydro_year_id: the same block and label
    expect_identical(ai_label(ai, hydro_year_id = 2000), lab)
})

test_that("label_year = 'start': the same block is labelled P44005.2000", {
    ai <- subset(PTclim05, plot == "P44005") |>
        moveYr(label_year = "start") |>
        wlai(label_year = "start")
    row <- subset(as.data.frame(ai), year == 2000)
    expect_identical(row$hydro_year_id, 2000L)
    lab <- ai_label(ai, year = 2000)
    expect_identical(lab, sprintf("AI = %s  [P44005.2000]", round(row$AI, 3)))
})
