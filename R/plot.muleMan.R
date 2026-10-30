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
    ## tree-biomass series of each core, processed without a terminal
    ## model (on these 2 plots x 2 trees x 2 cores no mixed model with plot,
    ## tree and core levels is well posed), with the columns muleMan pairs
    ## on: the series, year and the plot, tree and core labels
    trw <- Pchron |>
        modelFrame(form = NULL, to = 'cm',
                   MoreArgs = list(mp = c(2, 1, biom_param)))
    trw <- trw[, c('x', 'year', 'plot', 'tree', 'sample')]
    ## aridity-index fluctuations from a model with a random intercept per
    ## plot; hydrological years labeled by the year in which they end
    data(Prec, envir = environment())
    data(Temp, envir = environment())
    aif <- list(Prec, Temp) |>
        modelFrame(fn = list('moveYr', 'wlai'), lv = list('year', 'year'),
                   form = 'AI ~ year | plot', random = pdDiag(~ 1))
    ## Mantel correlograms between the two, for each core (100
    ## permutations to keep the example short)
    set.seed(1)
    mcomp <- muleMan(trw, aif, rd.var = 'x', nperm = 100)
    ## one correlogram per core; filled points mark significant lags
    plot(mcomp)
})
