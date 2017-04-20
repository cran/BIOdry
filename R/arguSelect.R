arguSelect <- structure(function#Argument selection
###Arguments of specific functions are selected from arbitrary numbers
###and varieties of arguments.
                     ##details<<Closures with ellipsis terms use this
                     ## function to extract and pass arguments to
                     ## other functions. Arguments in \code{MoreArgs}
                     ## lists are also extracted and stored again as
                     ## \code{MoreArgs} lists.
(
    rd = NULL, ##<<\code{NULL} or \code{data.frame}. Multilevel
               ##ecological data series. If \code{NULL} then this
               ##argument is ignored.
    fun = c('mapply','ringApply'), ##<< \code{character} or
                                   ##\code{NULL}. Vector of function
                                   ##names.
    ... ##<< Further arguments not necessarily contained in the
        ##processed function(s).
) {
    mx <- list(...)
    
    if('ref'%in%names(mx)){
        nl <- names(slitFrame(rd))
        refs <- levexp(mx[['ref']],nl)
        mx[['ref']] <- refs[nl]}
    
    mar <- 'MoreArgs'
    fn <- mx[['fn']]
    fun <- c(fun,fn)
    fca <- lapply(fun,
                  function(x)names(formals(x)))
    nfr <- unlist(fca)
    mx. <- mx[!names(mx)%in%mar]
    sel <- mx.[names(mx.)%in%nfr]
    s <- names(mx[[mar]])%in%nfr
    if(any(s))
        sel[[mar]] <- mx[[mar]][s]
    if(is.data.frame(rd))
        sel[['rd']] <- rd
    return(sel)
### \code{list} of selected arguments.
} , ex=function() {
    
    ##Multilevel ecological data series of tree-ring widths:
    data(Prings05,envir = environment())
    ## Radial increments measured on 2003:
    data(Pradii03,envir = environment())    
    
    ## Selection of arguments in some functions:
    ar1 <- arguSelect(fun = c('amod'),
                      only.dup = TRUE,
                      mp = c(0.5,1),
                      rf.t = 2003)
    str(ar1)
    
    ar2 <- arguSelect(fn = 'amod',
                      only.dup = TRUE,
                      mp = c(0.5,1),
                      rf.t = 2003)
    str(ar2)
    ar3 <- arguSelect(rd = Prings05,
                      fn = 'amod',
                      only.dup = TRUE,
                      mp = c(0.5,1),
                      rf.t = 2003)
    str(ar3)
    
    ar4 <- arguSelect(rd = Prings05,
                      fun = 'scacum',
                      sc.c = Pradii03,
                      MoreArgs = list(only.dup = TRUE,
                                      mp = c(0.5,1),
                                      rf.t = 2003))
    str(ar4)
    
    ar5 <- arguSelect(rd = Prings05,
                      fun = 'scacum',
                      ref = Pradii03,
                      rf.t = rep(2003:2011),
                      MoreArgs = list(only.dup = TRUE,
                                      mp = c(0.5,1)))
    str(ar5)    
    
})
