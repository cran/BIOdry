muleMan <- structure(function#Multilevel dendroclimatic correlograms
### Multivariate correlograms between TRW fluctuations and climatic
### fluctuations.
                     ##details<<Function \code{\link{mgram}} in
                     ##package \code{\link{ecodist}} is implemented to
                     ##compare the dendroclimatic fluctuations. Models
                     ##being compared should have common higher-level
                     ##factors (see example).
                     
                     ##references<< Lara W., F. Bravo,
                     ##D. Maguire. 2013. Modeling patterns between
                     ##drought and tree biomass growth from
                     ##dendrochronological data: A multilevel
                     ##approach. Agric. For. Meteorol.,
                     ##178-179:140-151.
(
    rd, ##<<\code{dataframe} or \code{\link{groupedData}}. TRW
        ##fluctuations such as that produced by
        ##\code{\link{modelFrame}}.
    cd, ##<<\code{dataframe} or
        ##\code{\link{groupedData}}. Aridity-index fluctuations such
        ##as that produced by \code{\link{modelFrame}}.
    rd.var = NULL, ##<<\code{character} or \code{NULL}. Column name of
                   ##the TRW fluctuations to be compared. If
                   ##\code{NULL} then the first column is processed.
    cd.var = NULL, ##<<\code{character} or \code{NULL}. Column name of
                   ##the aridity-index fluctuations to be compared. If
                   ##\code{NULL} then the first column is used.
    ... ##<<Further arguments in \code{\link{mgram}}
) {
    
    if(!is.data.frame(rd)){
        rd <- rd[['fluc']]
    }
    if(!is.data.frame(cd)){
        cd <- cd[['fluc']]
    }
    if(is.null(rd.var)){
        rd.var <- names(rd)[1L]
    }
    if(is.null(cd.var)){
        cd.var <- names(cd)[1L]
    }
                    
    ford <- function(cd,nm = 'year'){
        cd[do.call(order,
                   as.list(cd[, rev(c(nm,cClass(cd,'factor')))])),]}

    tmp0 <- slitFrame(rd, cClass(rd, 'factor'))
    cf <- function(x, cl = 'F'){
        if(cl%in%'I')
            cl <- 'integer'
        if(cl%in%'F')
            cl <- 'factor'
        cClass(x, cl)
    }
    flf. <- unlist(Map(function(x)
        any(x%in%cd[,cf(cd)]),
        rd[,cf(rd)]))
    fli. <- unlist(Map(function(x)
        any(x%in%cd[,cf(cd, 'I')]),
        rd[,cf(rd, 'I')]))
    fsl <- c(fli.,flf.)
    nrd <- names(fsl)[fsl]

    ncd <- cClass(cd, c('integer', 'factor'))
    
    fm <- function(x,...){
        tme <- merge(x,cd,by.x = nrd, by.y = ncd)
        tme <- na.omit(ford(tme))
        fny <- function(x,nm){
            data.frame(x[,nm])}
        tmw <- fny(tme,cd.var)
        tmt <- fny(tme,rd.var)
        spd <- dist(tmw)
        spp <- dist(tmt)
        man <- mgram(spp,spd,...)
        dman <- data.frame(man$'mgram')
        pnm <- c('mantelr','lag','pval')
        npnm <- names(dman)[!names(dman)%in%pnm]
        pn. <- c(pnm,npnm)
        dman <- dman[,pn.]
        return(dman)}
    
    tmp <- Map(function(x,...)fm(x,...),tmp0,...)
    ## lsdfn add levels in rd to mancor
    lsdfn <- function(mancor,rd){
        class(rd)
        rn <- do.call(rbind,mancor)
        code. <- rownames(rn)
        revn <- cClass(rd, 'factor')
        codes. <- do.call(rbind,
                          strsplit(code.,'\\.'))[
           ,1:length(revn)]
        codes. <- data.frame(codes.)
        codes. <- codes.[,rev(names(codes.))]
        codes. <- lapply(codes.,as.factor)
        names(codes.) <- revn
        rn <- cbind(rn,codes.)
        rownames(rn) <- NULL
        return(rn)}
    ## adding levels in rd to tmp
    tmp <- lsdfn(tmp,rd)    
    ## order data in tmp
    tmp <- ford(tmp,nm = 'lag')
    tmp <- groupedData(
        lmeForm(tmp,covar = 'lag'),data = tmp)
    md <- list(mmgram = tmp, call = sys.call())
    class(md) <- c('muleMan', class(md))
    return(md)
### \code{data.frame} object of multivariate correlations.
} , ex=function() {
    ##TRW chronology (mm) and inside-bark radii
    data(Pchron,envir = environment())
    data(Pradii03,envir = environment())
    ## TRW fluctuations:
    trwf <- modelFrame(Pchron,
                       sc.c = Pradii03,
                       rf.t = 2003,
                       log.t = TRUE)
    ## Climatic records:
    data(Temp,envir = environment())
    data(Prec,envir = environment())
    ## Aridity-index fluctuations:
    aif <- modelFrame(rd = list(Prec, Temp),
                      fn = list('moveYr','wlai'),
                      lv = list('year','year'),
                      form = 'lmeForm')

    ##Multivariate comparison:
    mcomp <- muleMan(trwf,
                        aif,
                     nperm = 10^3)
    str(mcomp)
    })
