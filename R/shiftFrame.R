shiftFrame <- structure(function#Shifting of ring-data frames
### Ring-data frames are reshaped into multilevel data frames (or vice versa). SI units of the processed chronologies can be changed.
##details<<Rows of the ring-data frames should be named with chronological years,and the columns should be labeled with the levels from sample design. Each of the levels is separated with dot (.), begining with highest level (i.e. ecorregion, climatic location, or plot)  and ending with the lowest level (usually core/replicate). For example, the code name of core 'a' in tree '2' on plot 'P16001' on ecorregion 'M1' will have the name: 'M1.P16001.2.a'. 
(
    rd, ##<<\code{data.frame}. Ring-data frame (see details), or
        ##multilevel data frame (see value).
    which.x = NULL, ##<<for the case of multilevel data
                    ##frames, \code{character} name of the column to be
                    ##reshaped.If NULL then the first \code{numeric}
                    ##column is processed.
    un = NULL ##<< \code{NULL}, one, or two \code{character} units of
              ##the metric system to record/transform the processed
              ##variables. One character records metric system; two
              ##characters with the form c(initial, final) change
              ##units in processed data. Defined SI units are
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
        x <- lapply(c(rd),as.data.frame)
        fr2n <- function(x)as.numeric(rownames(x))
        bindyr <- function(x){
            adyr <- data.frame(x,year = fr2n(rd))
            return(adyr)}
        x1 <- lapply(x,function(x)bindyr(x))
        dt <- na.omit(do.call(rbind,x1))
        names(dt) <- c('x','year')
        lev <- do.call(rbind,
                       strsplit(rownames(dt),split='\\.'))
        rlev <- ncol(lev) - 1# to exclude row number 
        lev <- lev[,1:rlev]
        nlev <- attributes(rd)[['nmLong']]
        if(is.null(nlev)){
            ins <- c('tree','sample')
            nmlv <- paste('level',1:(rlev - length(ins)),sep = '')
            nlev <- c(nmlv,ins)
        }
        dt[,nlev] <- lev
        dt[,nlev] <- lapply(dt[,nlev],as.factor)
        ordl <- c('x','year',rev(nlev))
        dt <- dt[,ordl]
        if(lu == 2)
            dt[,'x'] <- with(dt, x * chun(un[1],un[2]))
        rownames(dt)  <- NULL
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
            Reduce(function(x,y){merge(x,y,all=TRUE)},tomatch.)}
        rP <- fmatch(dsp)
        rownames(rP) <- rP[,'year']
        dt <- rP[,names(rP)[-1L]]
        dt <- dt[,order(names(dt))]
        if(lu == 2)
            dt <- chun(un[1],un[2]) * dt
        attributes(dt)[['nmLong']] <- rev(nfx)
    }
    attributes(dt)[['un']] <- un
    if(lu == 2)
        attributes(dt)[['un']] <- un[2]
    return(dt)
### If \code{rd} is a ring-data frame then output is a multilevel data frame with the reshaped variable in the first column and years on the second one, followed by factor-level columns from lowest level (core/replicate) to higest possible level. If \code{rd} is a multilevel data frame then the output is a ring-data frame (see details).
} , ex=function(){
    ##Multilevel data frame of tree-ring widths:
    data(Prings05,envir = environment())

    ## Reshaping multilevel data into a ring-data frame:
    pwide <- shiftFrame(Prings05)
    str(pwide)
    ## Reshaping the ring-data frame into initial multilevel data:
    plong <- shiftFrame(pwide)
    str(plong)
})
