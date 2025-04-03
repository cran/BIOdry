lmeForm <- structure(function#LME formula
### This function computes LME formulas from multilevel ecological
### data series (MEDS).
                     ##details<< Formulas of the form \code{resp ~ cov
                     ##| group} (see \code{\link[nlme]{groupedData}}
                     ##function) are computed from MEDS. The formulas
                     ##can be implemented by
                     ##\code{\link{modelFrame}} function to detrend MEDS
                     
                     ##references<< Pinheiro J. C.,
                     ##D. M. Bates. 2000. Mixed-effects models in S
                     ##and S-PLUS. Springer, New York.
(
    rd, ##<< \code{data.frame}. Multilevel ecological data series
    prim.cov = FALSE, ##<<\code{Logical}: should the LME formula only
                      ##be printed in primary covariate form: '~ cov'?
                      ##If FALSE then a complete form: 'resp ~ covar |
                      ##group' is formulated.
    resp = NULL, ##<<\code{NULL} or \code{character}. Column name of
                 ##the response. If NULL then the name of the first
                 ##numeric column of the MEDS is used.
    covar = NULL, ##<<\code{NULL} or \code{character}. Column name(s)
                  ##of the covariate(s). If \code{NULL} then the name
                  ##of the first time-unit column in the MEDS is used.
    lev.rm = NULL ##<< \code{NULL}, \code{character} or \code{numeric}
                  ##vector of levels in the MEDS to be removed from
                  ##the groups.
    
) {
    if(is.null(resp))
        resp <- cClass(rd, 'numeric')[1L]
    if(is.null(covar))
        covar <- cClass(rd, 'integer')[1L]        
    covar. <- paste('~',covar,sep = ' ')
    covar <- paste(resp,'~',covar,sep = ' ')
    f <- cClass(rd, 'factor')
    if(is.numeric(lev.rm))
        lev.rm <- f[lev.rm]
    nf <- rev(f[!f%in%lev.rm])
    sep. <- ' | '
    if(length(nf) == 0)
        sep. <- ''
    fc <- paste(nf,collapse = '/')
    fr <- paste(covar,fc,sep = sep.)
    fr <- formula(fr,showEnv = FALSE)
    if(prim.cov)fr <- covar.
    
    return(fr)
### \code{formula} with any of the forms: \code{resp ~ cov | group} or
### \code{~ cov}.
} , ex=function(){
    ##Multilevel ecological data series of tree-ring widths:
    data(Prings05,envir = environment())
    
    ## LME formula:
    form1 <- lmeForm(Prings05,prim.cov = FALSE)
    print(form1)
    ## removing the sample level from the formula
    form2 <- lmeForm(Prings05,lev.rm = 'sample')
    form2 <- lmeForm(Prings05,lev.rm = 1)
    
    ## groupedData object with the LME formula 
    gdata <- groupedData(lmeForm(Prings05,lev.rm = 1),
                         data = Prings05)
    plot(gdata,groups = ~ sample)
})
