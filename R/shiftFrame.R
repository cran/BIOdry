shiftFrame <- structure(function#MEDS formatting
### Ring-data frames (e.g. \code{dplR} chronologies) are formatted into
### multilevel ecological data series (or vice versa). 
                        ##details<< Correct formatting of ring-data
                        ##frames requires their row names to be
                        ##time-units labels (e.g. years). The column
                        ##names should be dot-separated labels
                        ##representing the hierarchy of ecological
                        ##factors, where the higher levels are defined
                        ##first and the lower levels after. For
                        ##example, code 'P16106.17' is the column name
                        ##of tree '17' in plot 'P16106'. SI units of
                        ##the processed chronologies can also be
                        ##changed.
                        
(
    rd, ##<<\code{data.frame}. Ring-data frame (see details), or
        ##multilevel ecological data series (MEDS).
    lev.nm = c('plot','tree','sample'), ##<<for the case of ring-data
                                        ##frames, \code{character}
                                        ##vector with names of the
                                        ##factor-level columns in the
                                        ##final MEDS, beginning with
                                        ##name of the highest level
                                        ##column and ending with name
                                        ##of the lowest level
                                        ##column. If \code{rd} is a
                                        ##MEDS then this argument is
                                        ##ignored.
    which.x = NULL, ##<<for the case of MEDS, \code{character} name of
                    ##the column to be reshaped into a ring-data
                    ##frame. If NULL then the first \code{numeric}
                    ##column is processed. If \code{rd} is a ring-data
                    ##frame then this argument is ignored.
    un = NULL ##<< \code{NULL}, one, or two \code{character} names of
              ##SI units to record/transform the processed
              ##variables. One character records metric system; two
              ##characters with the form c(initial, final) change SI
              ##units in the processed data. Defined SI units are
              ##micrometers 'mmm', milimeters 'mm', centimeters 'cm',
              ##decimeters 'dm', or meters 'm'. If NULL then no metric
              ##system is recorded.
) {
    fach <- c('factor','character')
    long <- any(sapply(rd,class)%in%fach)
    chun <- function(from,to){
        sm <- 10 ^ -c(6,3:0)
        un <- c('mmm','mm','cm','dm','m')
        names(sm) <- un
        eq <- sm[from]/sm[to]
        names(eq) <- to
        return(eq)}
    lu <- length(un)
    if(!long)
    {
        nm <- rep(names(rd),each=nrow(rd))
        splnm <- data.frame(
            do.call(rbind,
                    strsplit(nm,split = '\\.')))
        names(splnm) <- lev.nm
        splnm <- splnm[,rev(names(splnm))]
        yr <- as.numeric(rep(
            rownames(rd),ncol(rd)))
        dl <- unlist(c(rd),use.names = FALSE)
        dt <- na.omit(data.frame(x = dl,year = yr,splnm))
        rownames(dt) <- NULL
        
        if(lu == 2)
            dt[,'x'] <- with(dt, x * chun(un[1],un[2]))
        
    }
    else{
        nmx <- names(rd)
        n. <- sapply(rd,is.numeric)
        nux <- names(rd[,nmx[n.]])
        nfx <- names(rd[,!nmx%in%nux])
        nux1 <- nux[1]
        if(!is.null(which.x))nux1 <- which.x
        rd <- rd[,c(nux1,'year',nfx)]
        names(rd) <- c('x','year',nfx)
        ftosp <- lapply(rd[,rev(nfx)],as.factor)
        dsp <- split(rd,ftosp,drop = TRUE)
        dsp <- lapply(dsp,function(x)x[,c('year','x')])
        for(i in 1:length(dsp))
            names(dsp[[i]]) <- c('year',names(dsp[i]))
        fmatch <- function(tomatch.){
            Reduce(function(x,y){
                merge(x,y,all = TRUE)},tomatch.)}
        rP <- fmatch(dsp)
        rownames(rP) <- rP[,'year']
        dt <- rP[,names(rP)[-1L]]
        dt <- dt[,order(names(dt))]
        if(lu == 2)
            dt <- chun(un[1],un[2]) * dt
        attributes(dt)[['nmLong']] <- rev(nfx)
    }
    
    attributes(dt)[['un']] <- un[lu]
    return(dt)
### If \code{rd} is a ring-data frame then output is a MEDS. If
### \code{rd} is a MEDS then the output is a ring-data frame (see
### details).
} , ex=function(){
    ##tree-ring MEDS:
    data(Prings05,envir = environment())
    
    ## Formatting the MEDS into a ring-data frame:
    pwide <- shiftFrame(Prings05)
    str(pwide)
    ## Formatting the ring-data frame into a MEDS, and changing SI
    ## units of the rings from milimeters to micrometers:
    plong <- shiftFrame(pwide,un = c('mm','mmm'))
    str(plong)
})
