ringApply <- structure(function#Multilevel apply
### Wrapper of \code{\link{Map}} to apply functions on multilevel data
### series and preserve factor-level structure in the outputs.
                       ##details<< Other functions such as
                       ##\code{\link{rtimes}}, \code{\link{scacum}},
                       ##\code{\link{amod}}, or \code{\link{wlai}} can
                       ##be implemented.  Function arguments should be
                       ##formulated as suggested in
                       ##\code{\link{mapply}}, with constant
                       ##arguments being stored in a \code{MoreArgs}
                       ##list. This function is implemented by
                       ##\code{\link{modelFrame}} for recursive
                       ##modeling of MEDS.

                       ##references<< Lara, W., F. Bravo,
                       ##D. Maguire. 2013. Modeling patterns between
                       ##drought and tree biomass growth from
                       ##dendrochronological data: A multilevel
                       ##approach. Agric. For. Meteorol.,
                       ##178-179:140-151.
(
    rd, ##<<\code{data.frame}. Multileve ecological data series.
    lv = 1, ##<< {numeric} position, or {character} name, of an
            ##ecological factor in the processed MEDS.
    fn = 'scacum', ##<< \code{character} name of the function to be
                   ##evaluated (see details). Default 'scacum'
                   ##computes scaled-cumulative radii.
    ... ##<< Further arguments in the function being specified
        ##\code{fn} argument (see details)
)
{
    levs <- cClass(rd,'factor')
    emnt. <- unlist(Map(function(x)
        all(x%in%1:12) |
        all(x%in%month.abb),
        rd))
    if(any(emnt.))
        levs <- cClass(rd[
            names(emnt.)[!emnt.]], 'factor')
    if(is.character(lv)){
        if(!lv%in%levs){
            levs <- c(lv, levs)
        }
        if(lv%in%levs)
            lv <- match(lv, levs)
    }
    if(is.numeric(lv)){
        levs <- levs[lv:length(levs)]
    }
    cl1 <- slitFrame(rd, levs)
    
    fam <- function(x,...){
        do.call(fn,list(x,...))}
    
    cl2 <- Map(function(x,...)
        fam(x,...), cl1,...)
    nord <- names(cl2)[order(names(cl2))]
    cl3 <- cl2[nord]
    cl4 <- do.call(rbind,cl3)
    rownames(cl4) <- NULL
    return(cl4)
### \code{data.frame} object preserving initial factor-level columns.
} ,
ex=function() {
    
    ##Multilevel ecological data series (MEDS) of tree-ring widths:
    data(Prings05,envir = environment())
    ## Radial increments measured on 2003:
    data(Pradii03,envir = environment())    
    ## MEDS of monthly precipitation sums and average temperatures:
    data(PTclim05,envir = environment())
    
    ##Tree-level scaling of years of formation
    ##with 'rtimes' function:
    dfm1 <- ringApply(Prings05,
                      lv = 2,
                      fn = 'rtimes')
    str(dfm1)
    ##Relative time-units from year 1 to year 9:
    subset(dfm1,time%in%c(1:9,NA))
    
    ## Sample-level scaling of TRW chronologies around reference radii
    ## which were measured at 2003:
    dfm2 <- ringApply(dfm1,
                      lv = 'sample',
                      sc.c = Pradii03,
                      rf.t = 2003,
                      fn = 'scacum')
    str(dfm2)    
    ##Sample-level modeling of basal areas (mm2) via allometric
    ##scaling:
    dfm3 <- ringApply(dfm2,
                      lv = 'sample',
                      fn = 'amod',
                      MoreArgs = list(mp = c(2,1,0.25 * pi,2)))
    str(dfm3)
    
    ## Seasonal years from 'October' to 'September':
    cl1 <- ringApply(PTclim05,
                     lv = 'year',
                     fn = 'moveYr')
    tail(cl1,15)
    
    ##Year-level aridity indexes: 
    wl <- ringApply(cl1,
                    lv = 'year',
                    fn = 'wlai')
    str(wl)
    
    ## Plot of aridity-index fluctuations:
    d <- groupedData(lmeForm(wl),wl)
    plot(d)        
    
})
