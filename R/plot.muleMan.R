plot.muleMan <- structure(function #Plot muleMan objects
### Diagnostic Trellis plot for fluctuations in
### \code{\link{muleMan}} objects are obtained.
                        ##details<<One panel is drawn for each
                        ##correlogram. Panels are ordered by the grouping
                        ##levels from the outermost to the innermost (plot,
                        ##then tree, then core in the bundled data);
                        ##numeric labels are ordered as numbers. Filled
                        ##points mark lags with p-values below 0.05. The
                        ##axes are labeled "Lag" and "Mantel r" unless
                        ##\code{xlab} or \code{ylab} are given.
(
    x, ##<< An object inheriting from class \code{\link{muleMan}}.
    
    ... ##<< further arguments passed to the Trellis plot function.
) {
    mg <- x$'mmgram'
    dots <- list(...)
    if (!'xlab' %in% names(dots)) dots$xlab <- 'Lag'
    if (!'ylab' %in% names(dots)) dots$ylab <- 'Mantel r'
    ## grouping variables as written in the correlogram formula, innermost
    ## first (e.g. sample/tree/plot); panels are ordered outermost first,
    ## numeric-like labels as numbers
    gv <- all.vars(getGroupsFormula(mg))
    d <- as.data.frame(mg)
    key <- function(z) {
        z <- as.character(z)
        num <- suppressWarnings(as.numeric(z))
        if (!anyNA(num[!is.na(z)])) num else z
    }
    d <- d[do.call(order, c(lapply(d[rev(gv)], key), list(d$'lag'))), ,
           drop = FALSE]
    lab <- do.call(paste, c(d[gv], sep = '/'))
    d$'.groups' <- factor(lab, levels = unique(lab))
    gd <- groupedData(mantelr ~ lag | .groups, data = d,
                      order.groups = FALSE)
    do.call(plot,
            c(list(gd,
                   groups = gd$'pval' < 0.05,
                   pch = c(21,19),
                   abline = list(h = 0,
                                 lty = 2,
                                 lwd = 0.5,
                                 col = 'black')),
              dots))        ## A diagnostic Trellis plot.
} , ex=function() {
    ## ring-width chronologies (mm) of eight cores and the allometric
    ## parameters of the modelFrame example
    data(Pchron, envir = environment())
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    ## tree-biomass fluctuations
    trwf <- Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   log.t = FALSE, on.time = FALSE)
    ## aridity-index fluctuations; hydrological years labeled by the year
    ## in which they end
    data(Prec, envir = environment())
    data(Temp, envir = environment())
    aif <- list(Prec, Temp) |>
        modelFrame(fn = list('moveYr', 'wlai'), lv = list('year', 'year'),
                   form = 'lmeForm')
    ## Mantel correlograms between the two, for each core (100
    ## permutations to keep the example short)
    set.seed(1)
    mcomp <- muleMan(trwf, aif, nperm = 100)
    ## one correlogram per core; filled points mark significant lags
    plot(mcomp)
})
