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
    data(Pradii03,envir = environment())
    ## TRW fluctuations:
    trwf <- modelFrame(Pchron,
                       sc.c = Pradii03,
                       rf.t = 2003,
                       log.t = TRUE)
    ## Climatic records:
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
