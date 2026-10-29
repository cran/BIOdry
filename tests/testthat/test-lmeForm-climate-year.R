## Regression test: on the climate route of the muleMan help example
## (modelFrame on Prec and Temp with moveYr and wlai, then lmeForm), the
## aridity-index model must use 'year', the hydrological-year label (by
## default the year in which each October-September year ends), as BIOdry
## 0.9.1 did, so that muleMan pairs ring year t with the hydrological year
## ending in t. A time variable from metadata decides only when the user
## declared it with defineDataRoles(time = ...).
## Bundled data only: Prec, Temp, Pchron.
##
## Reference values: CRAN BIOdry 0.9.1 on the same route,
##   a <- modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
##                   lv = list("year", "year"), form = "lmeForm")
##   deparse(formula(a$model)); logLik(a$model); nlme::fixef(a$model)
## Its model data (AI by plot and year) equal ref-wlai-PTclim05-0.9.1.csv,
## which is checked below so that both versions fit identical inputs.
ref_091 <- list(
    formula = "AI ~ year",
    N = 100L,
    logLik = -17.2396616924,
    fixef = c(-18.9281119833, 0.00998498122232)
)

data(Prec, package = "BIOdry", envir = environment())
data(Temp, package = "BIOdry", envir = environment())
data(Pchron, package = "BIOdry", envir = environment())

aridity_fluc <- function() {
    modelFrame(rd = list(Prec, Temp), fn = list("moveYr", "wlai"),
               lv = list("year", "year"), form = "lmeForm")
}

test_that("the climate model uses 'year' and matches 0.9.1 on identical inputs", {
    aif <- aridity_fluc()
    m <- aif$model
    expect_equal(deparse(formula(m)), ref_091$formula)
    expect_equal(m$dims$N, ref_091$N)
    ## identical inputs: AI by plot and year as computed by 0.9.1
    md <- as.data.frame(m$data)
    md$plot <- as.character(md$plot)
    ref <- read.csv(test_path("ref-wlai-PTclim05-0.9.1.csv"), comment.char = "#")
    ref <- ref[!is.na(ref$AI), ]
    k <- merge(md[, c("plot", "year", "AI")], ref, by = c("plot", "year"),
               suffixes = c("", ".ref"))
    expect_equal(nrow(k), nrow(ref))
    expect_equal(k$AI, k$AI.ref, tolerance = 1e-10)
    ## identical fit
    expect_equal(as.numeric(logLik(m)), ref_091$logLik, tolerance = 1e-6)
    expect_equal(unname(nlme::fixef(m)), ref_091$fixef, tolerance = 1e-6)
})

## What muleMan computes from the two $fluc tables: for each core, the
## tree-ring and climate residuals are paired by year and plot, and a Mantel
## correlogram of their distances is computed (ecodist::mgram). mantelr, lag
## and ngroup are deterministic; pval and the bootstrap limits are not.
correlograms_by_hand <- function(rd, cd, shift = 0L) {
    rd <- as.data.frame(rd)
    cd <- as.data.frame(cd)
    cd$year <- cd$year + shift
    rd_var <- names(rd)[1L]
    cd_var <- names(cd)[1L]
    cores <- unique(rd[, c("plot", "tree", "sample")])
    do.call(rbind, lapply(seq_len(nrow(cores)), function(k) {
        s <- merge(cores[k, , drop = FALSE], rd, by = c("plot", "tree", "sample"))
        m <- na.omit(merge(s, cd, by = c("year", "plot"),
                           suffixes = c(".rd", ".cd")))
        rv <- if (rd_var %in% names(m)) rd_var else paste0(rd_var, ".rd")
        cv <- if (cd_var %in% names(m)) cd_var else paste0(cd_var, ".cd")
        g <- data.frame(ecodist::mgram(dist(m[[rv]]), dist(m[[cv]]),
                                       nperm = 0)$mgram)
        data.frame(core = paste(sort(c(as.character(cores$plot[k]),
                                       as.character(cores$tree[k]),
                                       as.character(cores$sample[k]))),
                                collapse = "|"),
                   g[, c("lag", "ngroup", "mantelr")])
    }))
}

## series key independent of which column holds which label
core_key <- function(d) {
    apply(d[, c("plot", "tree", "sample")], 1L, function(z)
        paste(sort(as.character(z)), collapse = "|"))
}

test_that("muleMan pairs ring year t with the hydrological year ending in t", {
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    trwf <- modelFrame(Pchron, to = "cm",
                       MoreArgs = list(mp = c(2, 1, biom_param)),
                       log.t = FALSE, on.time = FALSE)
    aif <- aridity_fluc()
    ## the climate fluctuations are labelled by the hydrological year's end year
    expect_true("year" %in% names(as.data.frame(aif$fluc)))
    expect_false("hydro_year_id" %in% names(as.data.frame(aif$fluc)))
    mm <- muleMan(trwf, aif)
    expect_s3_class(mm, "muleMan")
    out <- as.data.frame(mm$mmgram)
    out$core <- core_key(out)
    out <- out[order(out$core, out$lag), c("core", "lag", "ngroup", "mantelr")]
    ## same years: identical to muleMan
    same <- correlograms_by_hand(trwf$fluc, aif$fluc)
    same <- same[order(same$core, same$lag), ]
    expect_equal(nrow(same), nrow(out))
    expect_equal(same$core, out$core)
    expect_equal(same$lag, out$lag, tolerance = 1e-8)
    expect_equal(same$ngroup, out$ngroup)
    expect_equal(same$mantelr, out$mantelr, tolerance = 1e-8)
    ## climate years shifted by one: the test must notice the difference
    shifted <- correlograms_by_hand(trwf$fluc, aif$fluc, shift = 1L)
    shifted <- shifted[order(shifted$core, shifted$lag), ]
    differs <- nrow(shifted) != nrow(out) ||
        !isTRUE(all.equal(shifted$mantelr, out$mantelr, tolerance = 1e-8))
    expect_true(differs)
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    expect_s3_class(plot(mm), "trellis")
})
