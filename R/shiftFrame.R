shiftFrame <- structure(function#MEDS formatting
### dendroclimatic chronologies (trw, and climatic data) are formatted
### into multilevel ecological data series. SI units of continuous
### variables in the data can be transformed.
                        ##details<< Row names of dendroclimatic data
                        ##frames are time units (e.g. years). Column
                        ##names are dot-separated labels representing
                        ##the hierarchy of ecological or time-units
                        ##factors, where the higher levels are defined
                        ##first and the lower levels after. For
                        ##example, code 'P16106.17' is the column name
                        ##of core 'a' in tree '17' in plot
                        ##'P16106'. Labels containing monthly
                        ##abbreviations are also formatted.
                        
(
    rd, ##<<\code{data.frame} or \code{list}. Dendroclimatic
        ##chronology DC (see details) or list of two DCs
        ##(e.g. precipitation and temperature records), or multilevel
        ##ecological data series (MEDS).
    f.nm = NULL, ##<<\code{character} vector. In the case of formatting
                 ##ring-data frames, column names of the factors in
                 ##the new MEDS. If \code{NULL} then this argument is
                 ##recycled from attributes in \code{rd}. If such an
                 ##attribute is also \code{NULL} then a sequence of
                 ##codes (F1, F2, ..., Fn) is used.
    x.nm = names(rd)[1L], ##<<\code{character}. In the case of
                          ##formatting MEDS, name of the variable to be
                          ##reshaped. Default uses name of first
                          ##variable of \code{rd}.
    t.nm = 'year', ##<<\code{character}.In the case of formatting MEDS,
                   ##name of the time-units variable .
    ... ##<< Further arguments in \code{\link{mUnits}}.

) {

    fmnt <- function(dt){
        dtf <- dt[,cClass(dt,'factor')]
        emnt. <- unlist(Map(function(x)
            all(levels(x)%in%month.abb),
            dtf))
        if(any(emnt.)){
            dt[, names(emnt.)[emnt.]] <-
                factor(dt[, names(emnt.)[emnt.]],
                       levels = month.abb)
            nmd <- c(cClass(dt, 'integer'),
                     names(emnt.)[!emnt.])
            slld <- slitFrame(dt, nmd)
            rsr <- Map(function(x)
                x[do.call(order,
                          as.list(x[,cClass(x,'factor')])),],
                slld)
            dt <- do.call(rbind,rsr)
            rownames(dt) <- NULL}
        return(dt)}
    
    wide2long <- function(rd, f.nm, ...){ 
        sepl <- '\\.'
        hasdots <- length(grep(sepl, names(rd),value = TRUE)) <= 1
        if(hasdots) {
            stop('Column names must be dot-separated labels',
                 call. = FALSE)
        }
        nm <- rep(colnames(rd),each=nrow(rd))
        lev.1 <- as.data.frame(
            do.call(rbind, strsplit(nm,split = sepl)))
        if(is.null(f.nm)){
            f.nm <- attributes(rd)$'f.nm'
        }
        if(is.null(f.nm)){
            f.nm <- paste('F',1:ncol(lev.1), sep = '')
        }
        names(lev.1) <- f.nm
        lev.1 <- lev.1[,rev(names(lev.1))]
        yr <- as.numeric(rep(rownames(rd),ncol(rd)))
        x <- unlist(c(rd),use.names = FALSE)
        yr.nm <- attributes(rd)$'t.nm'
        if(is.null(yr.nm)){
            yr.nm <- 'year'
        }
        xvr <- attributes(rd)$'x.nm'
        if(is.null(xvr)){
            xvr <- 'x'
        }
        dt <- na.omit(data.frame(x, yr, lev.1))
        names(dt) <- c(xvr,yr.nm, f.nm)
        dt <- dt[,cClass(dt, 'all')]
        rownames(dt) <- NULL
        fns <- 'mUnits'
        dtu <- arguSelect(x = dt[,xvr], fun = fns, ...)
        dt[,xvr] <- do.call(fns, dtu)
        dt <- fmnt(dt)
        dt <- groupedData(formula = lmeForm(dt), data = dt)
        return(dt)}
    
    if(is.data.frame(rd)){
        iswide <- all(sapply(rd, is.numeric))
        if(iswide){
            dt <- wide2long(rd, f.nm, ...)
        }
        else{
            nfx <- cClass(rd, cl = 'factor')
            dtu <- arguSelect(x = rd[,x.nm], fun = 'mUnits', ...)
            rd[,x.nm] <- do.call('mUnits', dtu)
            ds <- split(rd, rd[,rev(nfx)], drop = TRUE)
            vec <- c(t.nm, x.nm)
            dsp <- lapply(ds,function(x)x[,vec])
            for(i in 1:length(dsp))
                names(dsp[[i]]) <- c(t.nm, names(dsp[i]))
            fmatch <- function(tomatch.){
                Reduce(function(x,y){
                    merge(x,y, by = t.nm, all = TRUE)},tomatch.)}
            rP <- fmatch(dsp)
            rownames(rP) <- rP[, t.nm]
            dt <- rP[,names(rP)[-1L]]
            dt <- dt[,order(names(dt))]
            attributes(dt)[c('f.nm','x.nm','t.nm')] <-
                list(nfx,x.nm,t.nm)
        }
    }
    else{
        rdl <- Map(function(x)
            wide2long(x, f.nm, ...), rd, ...)
        merdt <- Reduce(function(...)
            merge(...,
                  by = cClass(rdl[[1L]],
                              c('integer','factor')),
                  all=T), rdl)
        dt <- merdt[,cClass(merdt, 'all')]
        dt <- fmnt(merdt)
        dt <- dt[,cClass(dt, 'all')]
        ## dt <- groupedData(formula = lmeForm(dt), data = dt)

        }

    return(dt)
### When \code{rd} argument is a dendroclimatic chronology (see
### details) then the output is a \code{\link{groupedData}} object,
### and viceversa.
} , ex=function(){
    ##tree-ring widths formated as a groupedData object:
    data(Prings05,envir = environment())
    
    ## Formatting the groupedData object into a ring-data frame:
    pwide <- shiftFrame(Prings05, from = 'mm', to = 'mmm')
    str(pwide)
    ## Formatting the ring-data frame into a groupedData object, and
    ## changing SI units from micrometers to milimeters:
    plong <- shiftFrame(pwide,from = 'mmm', to = 'mm')
    plot(plong)
})
