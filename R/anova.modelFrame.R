anova.modelFrame <- structure(function #Compare modelFrame objects
###Models in \code{\link{modelFrame}} lists are compared with
###\code{\link{anova.lme}} method.

                              ##references<< Lara W., F. Bravo,
                              ##D. Maguire. 2013. Modeling patterns between
                              ##drought and tree biomass growth from
                              ##dendrochronological data: A multilevel
                              ##approach. Agric. For. Meteorol.,
                              ##178-179:140-151.

(
    object, ##<< an object inheriting from class "modelFrame".
    ..., ##<< other optional fitted model objects inheriting from
         ##classes "modelFrame", "lme", "lm", among other (see
         ##\code{\link{anova.lme}}).
    test, ##<< optional character string specifying the type of sum of
          ##squares to be used in F-tests for the terms in the model
          ##(see \code{\link{anova.lme}}).
    type,   ##<<optional character string specifying the type of sum
            ##of squares to be used in F-tests for the terms in the
            ##model (see \code{\link{anova.lme}}).
    adjustSigma, ##<< If TRUE and the estimation method used to obtain
                 ##object was maximum likelihood, the residual
                 ##standard error is multiplied by sqrt(nobs/(nobs -
                 ##npar)), converting it to a REML-like estimate (see
                 ##\code{\link{anova.lme}}).
    Terms, ##<< optional integer or character vector specifying which
           ##terms in the model should be jointly tested to be zero
           ##using a Wald F-test (see \code{\link{anova.lme}}).
    L, ##<< optional numeric vector or array specifying linear
       ##combinations of the coefficients in the model that should be
       ##tested to be zero (see \code{\link{anova.lme}}).
    verbose ##<< optional logical value. If TRUE, the calling
            ##sequences for each fitted model object are printed with
            ##the rest of the output, being omitted if verbose =
            ##FALSE (see \code{\link{anova.lme}}).
    
) {
    sc <- as.list(sys.call())[-1L]
    sch <- sc. <- sapply(sc,as.character)
    scn. <- sapply(names(sc),
                   function(x)x%in%"")
    if(length(scn.) != 0){
        schn <- names(sch)
        sc. <- sch[scn.]
        sch <- c(sc., schn[!scn.])
    }
    names(sc) <- sch
    sc[sc.] <- Map(as.character, sc[sc.] )
    sc[sc.] <- lapply(sc[sc.], get)
    names(sc) <- c('object', sch[2:length(sch)])
    for(i in 1:length(sc)){
        if(inherits(sc[[i]], 'modelFrame')){
            sc[[i]] <- sc[[i]]$'model' 
        }
    }
    aov <- do.call(anova, sc)
    rownames(aov) <- sc.
    return(aov)
### data frame inheriting from class "anova.lme".
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
    
    ## Fitting a single linear regression of the "tdForm" formula
    ## without random effects to the tree-biomass data:
    trwfl <- lm(log(x) ~ log(csx) + year,
                data = trwf$'model'$'data')
    ## Comparing model likelihoods with anova method:
    anova(trwf, trwfl)
})
