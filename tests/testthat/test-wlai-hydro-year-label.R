## Regression test: wlai() must label October-September hydrological years
## by the year in which they end when label_year = "end".
## Bundled data only: PTclim05 (monthly pr and tm, Jan 1955 - Dec 2005,
## locations P16106 and P44005).
##
## ref-wlai-PTclim05-0.9.1.csv holds the output of CRAN BIOdry 0.9.1 on the
## bundled PTclim05 with default arguments (plot, year label, AI). It was
## generated as follows, with 0.9.1 installed in its own library lib091
## (https://cran.r-project.org/src/contrib/Archive/BIOdry/ once 0.9.1 is no
## longer the current CRAN version):
##   library(BIOdry, lib.loc = lib091); data(PTclim05)
##   cl <- ringApply(PTclim05, lv = "year", fn = "moveYr")
##   d <- as.data.frame(ringApply(cl, lv = "year", fn = "wlai"))
##   d <- data.frame(plot = as.character(d$plot), year = as.integer(d$year),
##                   AI = d$AI)
##   d <- d[order(d$plot, d$year), ]
##   writeLines(c(
##       paste0("## wlai() on PTclim05 through ringApply(fn = \"moveYr\") ",
##              "then ringApply(fn = \"wlai\"),"),
##       "## computed with CRAN BIOdry 0.9.1 (default arguments). year = 0.9.1 label.",
##       "plot,year,AI",
##       sprintf("%s,%d,%s", d$plot, d$year,
##               ifelse(is.na(d$AI), "NA", sprintf("%.15g", d$AI)))),
##       "ref-wlai-PTclim05-0.9.1.csv")
data(PTclim05, package = "BIOdry", envir = environment())

oct_sep <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
             "Apr", "May", "Jun", "Jul", "Aug", "Sep")

## pr values of the hydrological year starting in October of year h at
## location p, read from PTclim05 in October-September order.
pr_oct_sep <- function(p, h) {
    yrs <- c(rep(h, 3), rep(h + 1, 9))
    vapply(seq_along(oct_sep), function(i) {
        PTclim05$pr[PTclim05$plot == p & PTclim05$year == yrs[i] &
                    as.character(PTclim05$month) == oct_sep[i]]
    }, numeric(1))
}

out_frame <- function(out) {
    d <- as.data.frame(unclass(out))
    d[order(d$plot, d$hydro_year_id), , drop = FALSE]
}

## Every complete block must hold the months of its hydrological year, and
## its year label must be the start year plus `offset` (0 start, 1 end).
expect_hydro_labels <- function(out, offset) {
    d <- out_frame(out)
    expect_equal(d$year, d$hydro_year_id + offset)
    blocks <- attr(out, "blocks")
    expect_length(blocks, 100L)
    for (nm in names(blocks)) {
        p <- sub("\\..*$", "", nm)
        h <- as.integer(sub("^.*\\.", "", nm))
        expect_equal(unname(blocks[[nm]]$cd$pr), pr_oct_sep(p, h), info = nm)
    }
}

## Every warning must be the moveYr/wlai mismatch message for the two
## conventions given, and there must be at least one.
expect_mismatch_warnings <- function(w, in_label, wl_label) {
    expect_gte(length(w), 1L)
    msg <- sprintf(paste0("The 'year' column of 'cd' labels hydrological ",
                          "years by their %s year (moveYr(label_year = '%s')), ",
                          "but wlai() uses label_year = '%s'; wlai's ",
                          "label_year decides the output 'year' column."),
                   in_label, in_label, wl_label)
    expect_true(all(w == msg))
}

test_that("label_year = 'end' labels each hydrological year by its end year", {
    out <- wlai(PTclim05, ini.mnt = "Oct", label_year = "end")
    expect_hydro_labels(out, 1L)
    ## October 1998 to September 1999 at P44005 is labelled 1999
    d <- out_frame(out)
    expect_equal(d$year[d$plot == "P44005" & d$hydro_year_id == 1998], 1999)
})

test_that("label_year = 'start' labels each hydrological year by its start year", {
    out <- wlai(PTclim05, ini.mnt = "Oct", label_year = "start")
    expect_hydro_labels(out, 0L)
    d <- out_frame(out)
    expect_equal(d$year[d$plot == "P44005" & d$hydro_year_id == 1998], 1998)
})

test_that("the default label_year is 'end'", {
    expect_equal(out_frame(wlai(PTclim05, ini.mnt = "Oct")),
                 out_frame(wlai(PTclim05, ini.mnt = "Oct", label_year = "end")))
})

test_that("label_year applies on the documented moveYr route", {
    ## ringApply help example: seasonal years with moveYr, then wlai
    cl_start <- ringApply(PTclim05, lv = "year", fn = "moveYr", label_year = "start")
    w <- capture_warnings(
        out <- ringApply(cl_start, lv = "year", fn = "wlai", label_year = "end"))
    expect_mismatch_warnings(w, "start", "end")
    d <- out_frame(out)
    expect_equal(d$year, d$hydro_year_id + 1)

    cl_end <- ringApply(PTclim05, lv = "year", fn = "moveYr", label_year = "end")
    w <- capture_warnings(
        out <- ringApply(cl_end, lv = "year", fn = "wlai", label_year = "end"))
    expect_length(w, 0L)
    d <- out_frame(out)
    expect_equal(d$year, d$hydro_year_id + 1)
})

test_that("a moveYr/wlai label mismatch warns and wlai's label_year decides", {
    cl_end <- ringApply(PTclim05, lv = "year", fn = "moveYr", label_year = "end")
    w <- capture_warnings(
        out <- ringApply(cl_end, lv = "year", fn = "wlai", label_year = "start"))
    expect_mismatch_warnings(w, "end", "start")
    d <- out_frame(out)
    expect_equal(d$year, d$hydro_year_id)
})

test_that("the default moveYr -> wlai route gives no warning and end-year labels", {
    cl <- ringApply(PTclim05, lv = "year", fn = "moveYr")
    w <- capture_warnings(out <- ringApply(cl, lv = "year", fn = "wlai"))
    expect_length(w, 0L)
    d <- out_frame(out)
    expect_equal(d$year, d$hydro_year_id + 1)
})

test_that("the default route gives 0.9.1's labels and AI on every block", {
    ref <- read.csv(test_path("ref-wlai-PTclim05-0.9.1.csv"), comment.char = "#")
    cl <- ringApply(PTclim05, lv = "year", fn = "moveYr")
    d <- out_frame(ringApply(cl, lv = "year", fn = "wlai"))
    d$plot <- as.character(d$plot)
    m <- merge(ref, d, by = c("plot", "year"), suffixes = c(".ref", ""))
    expect_equal(nrow(m), nrow(ref))
    expect_equal(nrow(m), nrow(d))
    expect_equal(is.na(m$AI), is.na(m$AI.ref))
    ok <- !is.na(m$AI.ref)
    expect_equal(m$AI[ok], m$AI.ref[ok], tolerance = 1e-10)
})

test_that("label_year has no effect on calendar years", {
    out_start <- wlai(PTclim05, label_year = "start")
    out_end <- wlai(PTclim05, label_year = "end")
    expect_identical(out_start, out_end)
    d <- as.data.frame(unclass(out_start))
    expect_false("hydro_year_id" %in% names(d))
    expect_equal(sort(unique(d$year)), 1955:2005)
})
