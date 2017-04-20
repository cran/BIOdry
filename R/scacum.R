scacum <- structure(function#Cummulative-scaled sums
### This function computes cummulative and scaled sums of time-series
### replicates.
                    ##details<< Cummulative sums of time-series
                    ## replicates (e.g. tree-ring widths) are scaled
                    ## around reference values (e.g. tree radii).
(
    x, ##<<\code{numeric} vector of time-series replicates with names
       ##of the vector being time units.
    sc.c = NA,   ##<<\code{numeric} constant. Scaling constant.  If
                 ##\code{NA} then the computed cumulative sums are not
                 ##scaled.
    rf.t = NA ##<<\code{NA}, or \code{numeric} constant. Reference
           ##time of the scaling constant. If \code{NA} then
           ##maximum time in vector-name range is used.
) {
    csn. <- FALSE
    if(is.data.frame(x)){
        csnu <- cClass(x, 'numeric')
        csn <- c(cClass(x, 'integer'), cClass(x, 'factor'))
        csn. <- length(csn)!=0
        csn.. <- csn[!csn%in%'csx']
        cd <- x
        x <- x[,'x']
        names(x) <- cd[,'year']}
    
    if(is.null(names(x)))
        stop('NULL labels in x', call. = FALSE)
        xcum <- cumsum(x)
        if(is.na(rf.t))
            rf.t <- max(as.numeric(names(x)))
            inc <- 0
            if(!is.na(sc.c))
                inc <- sc.c - xcum[as.character(rf.t)]
                csx <- xcum + inc
                if(any(csx < 0,na.rm = TRUE))
                    csx <- xcum
                    xd <- data.frame(x,csx)
                    
                    if(csn.&& length(csnu) > 1){
                        xd <- cd[,csnu]
                        xd[,'csx'] <- csx }
                    if(csn.)
                        xd <- cbind(xd,cd[,csn..])
                        
                        return(xd)
### data frame with the original vector, and its scaled-cummulative sums.
} , ex=function() {
    x <- c(0.79,0.32,0.53,0.43,0.18)
    names(x) <- 1948:1952
    scacum(x,sc.c = 4,rf.t = 1951)
    
    ##If sc.c = NA then cummulative values are scaled arround
    ##max(cumsum(x)):
    max(cumsum(x))
    scacum(x,NA,1951)
})
