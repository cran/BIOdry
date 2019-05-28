plot.muleMan <- structure(function #Plot muleMan objects
### Diagnostic Trellis plot for fluctuations in
### \code{\link{muleMan}} objects are obtained.
(
    x, ##<< An object inheriting from class \code{\link{muleMan}}.
    
    ... ##<< further arguments passed to the Trellis plot function.
) {
    plot(x$'mmgram',
         groups = ifelse(x$'mmgram'$'pval' < 0.05, TRUE, FALSE),
         pch = c(21,19),
         abline = list(h = 0,
                       lty = 2,
                       lwd = 0.5,
                       col = 'black'),
         ...)        ## A diagnostic Trellis plot.
} , ex=function() {
    ##TRW chronology (mm) and inside-bark radii
    data(Pchron,envir = environment())
    
    ## Parameters of allometric model to compute Diameter at Breast
    ## Height over bark (DBH, cm) from diameter inside bark (dib, cm)
    ## and Total Tree Biomass (TTB, kg tree -1 ) from DBH (Lara
    ## et. al. 2013):
    biom_param <- c(2.87, 0.85, 0.05, 2.5)

    ## Modeling tree-biomass fluctuations while accounting for
    ## within-plot source variability (see defaults in "modelFrame"
    ## function)
    trwf <- modelFrame(Pchron,
                       to = 'cm',
                       MoreArgs = list(mp = c(2,1, biom_param)),
                       log.t = FALSE,
                       on.time = FALSE)
    ## Climatic Records:
    data(Temp,envir = environment())
    data(Prec,envir = environment())
    ## Aridity-index fluctuations:
    aif <- modelFrame(rd = list(Prec, Temp),
                      fn = list('moveYr','wlai'),
                      lv = list('year','year'),
                      form = 'lmeForm')

    ##Multivariate comparison:
    mcomp <- muleMan(trwf,
                        aif,
                     nperm = 10^3)
    
    plot(mcomp, grid = FALSE)    
})
