modelFrame <- structure(function #Dendroclimatic-fluctuations modeling
### This function develops recursive evaluation of functions for
### one-level modeling (FOLM) and LME detrending of dendroclimatic
### chronologies.
                        ##details<< Defaults model fluctuations in
                        ##tree-ring width chronologies via recursive
                        ##implementation of four FOLM:
                        ##\code{\link{rtimes}}, \code{\link{scacum}},
                        ##\code{\link{amod}}, and
                        ##\code{\link{frametoLme}}. Nevertheless,
                        ##other FOLM can be implemented to model
                        ##aridity-index fluctuations(see example with
                        ##climatic data). Processed chronologies are
                        ##detrended with \code{\link{lme}} function
                        ##and other \code{\link{nlme}} methods
                        ##. Internal algorithm uses
                        ##\code{\link{shiftFrame}}
                        ##\code{\link{arguSelect}} and
                        ##\code{\link{ringApply}}
                        ##functions. Consequently, arguments that are
                        ##not iterated over factor-level labels in the
                        ##processed data are specified in 'MoreArgs'
                        ##lists (see examples). Arguments in
                        ##\code{modelFrame} objects can be updated
                        ##with \code{\link{update}} function.

                        ##references<< Lara W., F. Bravo,
                        ##D. Maguire. 2013. Modeling patterns between
                        ##drought and tree biomass growth from
                        ##dendrochronological data: A multilevel
                        ##approach. Agric. For. Meteorol.,
                        ##178-179:140-151.
(
    rd, ##<<\code{data.frame} or \code{list}. Dendroclimatic
        ##chronology or Multilevel ecological data series.
    fn = list('rtimes','scacum','amod'), ##<< \code{list}.  Names of
                                         ##the functions for one-level
                                         ##modeling to be recursively
                                         ##implemented.

    lv = list(2,1,1), ##<< \code{list}. \code{numeric} positions in
                      ##the factor-level labels of \code{rd} to
                      ##implement the one-level functions. If
                      ##\code{rd} is a MEDS, then \code{character}
                      ##names of the factor-level columns.
    form = 'tdForm',   ##<<\code{character} or \code{NULL}. Name of a
                       ##detrending formula.  Two in-package
                       ##methods are available: the default
                       ##\code{\link{tdForm}} or
                       ##\code{\link{lmeForm}}.
    
    ... ##<< Further arguments in \code{\link{mUnits}}, or in the
        ##functions for one-level modeling, or in the
        ##\code{\link{lme}} function/methods, or in the detrending
        ##formula.
) {
    lse <- list(...)
    mln <- length(lv)
    iswide <- all(sapply(rd, is.numeric))
    islist <- class(rd)%in%'list'
    if(any(iswide, islist)){
        rd <- shiftFrame(rd)
    }
    fns <- 'mUnits'
    if(any(names(lse)%in%names(formals(fns)[-1L]))){
        nmu <- cClass(rd, 'numeric')
        rdu <- arguSelect(x = rd[,nmu], fun = fns, ...)
        rd[,nmu] <- do.call(fns, rdu)
        if('sc.c'%in%names(lse)){
            sca <- arguSelect(x = lse$'sc.c', fun = fns, ...)
            lse[['sc.c']] <- do.call(fns, sca)
        }
    }
    mar <- 'MoreArgs'
    ls. <- lapply(lse,class)%in%'list'
    yls <- Map(unlist,lse[ls.])
    yls[c('fn','lv')] <- list(fn,lv)
    nma <- yls[!names(yls)%in%mar]
    lsp <- lse[!names(lse)%in%names(yls)]
    s <- names(lse)%in%mar
    if(any(s))
        lsp[[mar]] <- lse[[mar]]
    ar <- list()
    mln <- length(nma[[1L]])
    for(i in 1:mln){
        lsl <- lapply(nma, '[[', i)
        lt <- list(rd, fun = 'ringApply')
        nl <- unlist(Map(levels,
                         rd[cClass(rd,'factor')]))
        spd <- function(x){
            unlist(strsplit(x, '\\.'))}
        my <- unlist(Map(function(x)
            !is.null(names(x)) &&
            spd(names(x))%in% nl, lsp))
        if(any(my)) {
            lsp[names(lsp)[my]] <- Map(function(x)
                levexp(x, rd),lsp[names(lsp)[my]])}
        lst <- c(lsl, lsp, lt)
        ar[[i]] <- do.call('arguSelect', lst)
        rd <- do.call('ringApply', ar[[i]])
    }
    arl <- arguSelect(rd,
                      fun = c('frametoLme','lme', form),...)
    arl[['form']] <- form
    rd <- do.call('frametoLme',arl)
    rd[['call']] <- sys.call()
    class(rd) <- c('modelFrame', class(rd))
    return(rd)
### Threefold list with fluctuations in \code{fluc},
### {\link{groupedData}} object in \code{model}, and model call in
### \code{call}.
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
            
    ## Climatic records:
    data(Temp,envir = environment())
    data(Prec,envir = environment())
    ## Aridity-index fluctuations:
    aif <- modelFrame(rd = list(Prec, Temp),
                      fn = list('moveYr','wlai'),
                      lv = list('year','year'),
                      form = 'lmeForm')
    summary(aif$'model')
})
