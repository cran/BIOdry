## Regression test: on the documented route ringApply(fn = "moveYr") then
## ringApply(fn = "wlai"), each October-September block must be computed in
## October-September order, as on the direct route wlai(ini.mnt = "Oct").
## Bundled data only: PTclim05. Reference value from BIOdry 0.9.1.
data(PTclim05, package = "BIOdry", envir = environment())

ai_by_block <- function(out) {
    d <- as.data.frame(unclass(out))
    d <- d[, c("plot", "hydro_year_id", "AI")]
    d$plot <- as.character(d$plot)
    d[order(d$plot, d$hydro_year_id), , drop = FALSE]
}

direct_route <- function() ai_by_block(wlai(PTclim05, ini.mnt = "Oct"))

moveyr_route <- function() {
    cl <- ringApply(PTclim05, lv = "year", fn = "moveYr")
    ai_by_block(ringApply(cl, lv = "year", fn = "wlai"))
}

test_that("P44005 October 1998 - September 1999 gives the 0.9.1 AI on both routes", {
    for (d in list(direct_route(), moveyr_route())) {
        ai <- d$AI[d$plot == "P44005" & d$hydro_year_id == 1998]
        expect_equal(ai, 1.527597, tolerance = 1e-6)
    }
})

test_that("the moveYr route and the direct route agree on every block", {
    a <- direct_route()
    b <- moveyr_route()
    m <- merge(a, b, by = c("plot", "hydro_year_id"), suffixes = c(".direct", ".moveYr"))
    expect_equal(nrow(m), nrow(a))
    expect_equal(nrow(m), nrow(b))
    expect_equal(is.na(m$AI.moveYr), is.na(m$AI.direct))
    ok <- !is.na(m$AI.direct)
    expect_gt(sum(ok), 90L)
    expect_equal(m$AI.moveYr[ok], m$AI.direct[ok], tolerance = 1e-10)
})
