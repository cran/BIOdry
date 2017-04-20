moveYr <- structure(function#Seasonal years
### Monthly records in time-series replicates (usually of climate) are
### labeled for the years can begin in a month other than January.
                    ##details<<\code{character} months as defined in
                    ##\code{\link{month.abb}} or \code{\link{month.name}}.
(
    cd, ##<<\code{data.frame}. Multilevel ecological data series or
        ##\code{numeric} vector of repeated years with vector names
        ##belonging to \code{month.abb}.
    ini.mnt = 'Oct' ##<<\code{character}, or \code{numeric} from 1 to
                    ##12. Initial month of the seasonal year. If
                    ##\code{character} then the months are built-in
                    ##constants in R-package \code{base}. Default
                    ##\code{'Oct'} makes the years begin in October,
                    ##for example.
) {
    
    chn <- function(tmp){
        tmp. <- as.character(tmp)
        nm <- 1:12
        fm <- 'month.abb'
        mna <- tmp%in%month.abb[nm]
        if(!all(mna))
            fm <- 'month.name'
        names(nm) <- get(fm)[nm]
        if(!is.numeric(tmp)){
            tmp <- nm[tmp][tmp.]}
        names(tmp) <- get(fm)[tmp]
        return(tmp)}
    
    isdf <- is.data.frame(cd)
    if(isdf){
        ny <- cd
        cd <- cd[,'year']
        dt <- ny
        dtf <- dt[,cClass(dt,'factor')]
        emnt. <- unlist(Map(function(x)
            all(levels(x)%in%month.abb),
            dtf))
        names(emnt.)[emnt.]

        names(cd) <- chn(ny[,names(emnt.)[emnt.]])}
    if(!isdf)
        names(cd) <- chn(names(cd))
        ini.mnt <- chn(ini.mnt)
        mn <- 1:12
        ncd <- ifelse(
            mn[as.numeric(names(cd))] >= mn[ini.mnt],
               ifelse(
                   mn[ini.mnt] > mn[5],
                   cd + 1, cd),
               ifelse(mn[ini.mnt] <= mn[5],
                      cd - 1, cd))
        
        if(isdf){
            ny[,'year'] <- ncd
            ny[,'month'] <- factor(month.abb[
                as.numeric(names(cd))],
                levels = month.abb[as.numeric(names(cd))])
            ny <- ny[cClass(ny)]
        }    
        if(!isdf){
            ny <-  ncd
            names(ny) <- month.abb[as.numeric(names(cd))]
        }
        return(ny)
### \code{data.frame} object with the months being \code{numeric}
### values and the years beginning at \code{ini.mnt} argument.
} , ex=function() {
    ## Climatic records of monthly precipitation sums and monthly
    ## average temperatures
    data(PTclim05,envir = environment())
    
    ## Making the year 1955 in plot 'P16106' to begin on 'April'
    cl1 <- slitFrame(PTclim05,c('year','plot'))[[1]]
    cl2 <- moveYr(cl1,ini.mnt = 'Mar')
    head(cl2)
    
    ## a simple vector of years
    yr <- rep(2005,12)
    names(yr) <- month.abb[1:12]
    moveYr(yr)
    
})
