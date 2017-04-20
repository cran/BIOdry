plot.modelFrame <- structure(function #Plot modelFrame objects
### Diagnostic Trellis plot for fluctuations in
### \code{\link{modelFrame}} objects are obtained.
(
    x, ##<< An object inheriting from class \code{\link{modelFrame}}.
    
    ... ##<< further arguments passed to the Trellis plot function.
) {
        plot(x$'fluc',  
             type = 'l',
             ## grid = FALSE,
             abline = list(h = 0,
                           lty = 2,
                           lwd = 0.5,
                           col = 'gray30'),
             ...)
        ## A diagnostic Trellis plot.
} , ex=function() {
    ##TRW chronology (mm) and reference inside-bark radii (mm)
    ##measured at 2003:
    data(Pchron,envir = environment())
    data(Pradii03,envir = environment())
    ## Tree-ring width fluctuations:
    trwf <- modelFrame(Pchron,
                       sc.c = Pradii03,
                       rf.t = 2003,
                       log.t = TRUE)
    plot(trwf, grid = FALSE)

})
