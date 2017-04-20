rtimes <- structure(function#Time-units synchronization
### Unique observations in time-series replicates are excluded
                    ## details<<This function is used to enhance
                    ## convergence of mixed-effects parameters during
                    ## detrending processes of multilevel ecological
                    ## data series (see \code{\link{modelFrame}}
                    ## function).
(
    x, ##<< multilevel ecological data series containing a column of
       ##time units, or \code{numeric} vector with names representing
       ##the time units.
    only.dup = TRUE ##<< \code{logical}. Extract only duplicated
                    ##times.  If TRUE then unique times are replaced
                    ##with NA. If all computed times are unique then
                    ##this argument is ignored.
) {
    csn. <- FALSE
    if(is.data.frame(x)){
        csnu <- cClass(x, 'numeric')
    csn <- c(cClass(x, 'integer'), cClass(x, 'factor'))
        csn. <- length(csn)!=0
        csn.. <- csn[!csn%in%'time']
        cd <- x
        x <- x[,'x']
        names(x) <- cd[,'year']}
    
    n <- as.numeric(names(x))
    time <- abs(min(n) - n - 1)
    da <- data.frame(x,time)
    ## return(csn.&& length(csnu) > 1)
    if(csn.&& length(csnu) > 1){
        ## csn.. <- csn[!csn%in%'time'] #
        da <- cd[,csnu]
        da[,'time'] <- time }
    rownames(da) <- 1:nrow(da)
    dp <- duplicated(da[,'time'])
    uni <- with(da,!time%in%da[,'time'][dp])
    if(only.dup&any(dp))
        da[uni,'time'] <- NA
    if(csn.)
        da <- cbind(da,cd[,csn..])
    return(da)
### \code{data.frame} object with the initial vector and its time
### units.
} , ex=function(){
    ## row names of a vector
    fy <- function(y,span){(y - span):y}
    x <- c(fy(2005,5),fy(2007,10)) 
    ## (not run) Simulating the vector
    r <- abs(rnorm(length(x)))
    names(r) <- x
    ## (not run) computing the synchronized times:
    rtimes(r,only.dup = TRUE)        
    ## (not run) Extracting only duplicated times:
    na.omit(rtimes(r,only.dup = TRUE))
})
