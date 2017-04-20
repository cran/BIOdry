cClass <- structure(function# Column-class extraction.
### Column names of multilevel data sets are extracted according to
### three classes: \code{numeric} values, \code{integer} sequences,
### and \code{factor} levels.
(
    rd, ##<<\code{data.frame}. Multilevel  data series.
    cl = 'all' ##<<\code{character} or \code{NULL}. Character vector
                  ##of classes to be considered. These can be
                  ##'numeric', 'integer', or 'factor'. If \code{'all'}
                  ##then all column names of \code{rd} are extracted.
) {
    fnm <- function(nml){names(nml)[nml]}
    nml <- sapply(rd, is.numeric)
    nm <- fnm(nml) 
    inl <- sapply(rd[,nm],function(x)
        all(floor(x) == x, na.rm = TRUE))
    in. <- fnm(inl)
    nu <- nm[!nm%in%in.]  
    fct <- sapply(rd, is.factor)
    fc <- fnm(fct)
    cls <- list(numeric = nu,
                integer = in.,
                factor = fc)
    if(cl[1L]%in%'all')
    cl <- names(cls)
    cls <- unlist(cls[cl])
    return(cls)
### \code{character} names.
        
} , ex=function() {
    ##Multilevel data frame of tree-ring widths:
    data(Prings05,envir = environment())
    ## Names of variables in Prings05 data containing numeric classes:
    cClass(Prings05, 'numeric') # 'x'
    ## Names of variables containing time units: 
    cClass(Prings05, 'integer') # 'year'
    ## Names of variables containing factors: 
    cClass(Prings05, 'factor') # 'sample', 'tree', 'plot'
    
})
