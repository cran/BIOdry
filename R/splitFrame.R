splitFrame <- structure(function#Multilevel splitting 
### This function splits a multilevel ecological data series (MEDS) based on
### the levels of an ecological factor.
                        ##details<< This function is implemented by
                        ##\code{\link{ringApply}} and
                        ##\code{\link{modelFrame}} functions to
                        ##develop multilevel modeling of MEDS.
(
    rd, ##<<\code{data.frame}. Multilevel ecological data series.
    lv = 1 ##<< \code{Numeric} or \code{character}. Position number of
           ##the ecological factor(s) in the MEDS, or correspondent
           ##column name(s), to split the data. If the splitting
           ##column is not a factor, the character name of the column
           ##should be used.
) {
    
    levs <- colclass(rd,TRUE)[['fac']]
    if(is.numeric(lv)) lv <- levs[lv]
    
    ff <- function(x,lv,as.num = TRUE){
        x[,lv] <- as.character(x[,lv])
        if(as.num)
            x[,lv] <- as.numeric(x[,lv])
        return(x)}
    if(is.null(lv))
        lv <- colclass(rd,TRUE)[['fac']][1]
        nf <- is.numeric(rd[,lv])
        if(nf)
            rd <- ff(rd,lv,as.num = FALSE)
            nmx <- names(rd)
            n. <- sapply(rd,is.numeric)
            nux <- names(rd)[n.]
            lt <- rev(names(rd)[!n.])
            lt <- lt[1:grep(lv,lt)]
            rds <- split(rd,rd[,lt],drop = TRUE)
            if(nf) rds <- Map(function(x)ff(x,lv),rds)
            return(rds)    
### \code{list} of \code{data.frame} objects.
} , ex=function() {
    ##Dendrochronological MEDS:
    data(Prings05,envir = environment())
    
    ## split the MEDS by levels contained in its second ecological
    ## factor:
    spl <- splitFrame(Prings05,2)
    str(spl)
    ## split the data by levels defined in the 'year' factor:
    spl <- splitFrame(Prings05,'year')
    str(spl)
})
