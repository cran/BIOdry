summary.modelFrame <- structure(function #summarize a modelFrame object
### A summary of a \code{\link{modelFrame}} object is obtained.
(
    object, ##<< an object inheriting from class
            ##\code{\link{modelFrame}}.
    ... ##<< additional optional arguments passed to
        ##\code{\link[nlme]{summary.lme}} method.
    
) {
        summary(object$'model', ...)
        ## A summary model.
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
    ## function):
    ## \donttest{
    ## trwf <- modelFrame(Pchron,
    ##                    to = 'cm',
    ##                    MoreArgs = list(mp = c(2,1, biom_param)),
    ##                    log.t = TRUE,
    ##                    on.time = TRUE)
    ## summary(trwf)
    ## }
})
