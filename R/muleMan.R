muleMan <- structure(function#Multilevel dendroclimatic correlograms
### Multivariate correlograms between TRW fluctuations and climatic
### fluctuations.
                     ##details<<Function \code{\link[ecodist]{mgram}}
                     ##is implemented to compare the dendroclimatic
                     ##fluctuations. Models being compared should have
                     ##common higher-level factors (see example).
                     
                     ##references<< Lara W., F. Bravo,
                     ##D. Maguire. 2013. Modeling patterns between
                     ##drought and tree biomass growth from
                     ##dendrochronological data: A multilevel
                     ##approach. Agric. For. Meteorol.,
                     ##178-179:140-151.
(
    rd, ##<<\code{dataframe} or \code{\link[nlme]{groupedData}}. TRW
        ##fluctuations such as that produced by
        ##\code{\link{modelFrame}}.
    cd, ##<<\code{dataframe} or
        ##\code{\link[nlme]{groupedData}}. Aridity-index fluctuations such
        ##as that produced by \code{\link{modelFrame}}.
    rd.var = NULL, ##<<\code{character} or \code{NULL}. Column name of
                   ##the TRW fluctuations to be compared. If
                   ##\code{NULL} then the first column is processed.
    cd.var = NULL, ##<<\code{character} or \code{NULL}. Column name of
                   ##the aridity-index fluctuations to be compared. If
                   ##\code{NULL} then the first column is used.
    ... ##<<Further arguments in \code{\link[ecodist]{mgram}}
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
        rd[,cf(rd), drop = FALSE]))
    fli. <- unlist(Map(function(x)
        any(x%in%cd[,cf(cd, 'I')]),
        rd[,cf(rd, 'I'), drop = FALSE]))
    fsl <- c(fli.,flf.)
    nrd <- names(fsl)[fsl]

    ncd <- cClass(cd, c('integer', 'factor'))
    
    fm <- function(x,...){
        ## value columns present in both inputs (e.g. plot.res) get suffixes
        tme <- merge(x,cd,by.x = nrd, by.y = ncd, suffixes = c('.rd', '.cd'))
        tme <- na.omit(ford(tme))
        fny <- function(x,nm){
            data.frame(x[,nm])}
        cdv <- if(cd.var %in% names(tme)) cd.var else paste0(cd.var, '.cd')
        rdv <- if(rd.var %in% names(tme)) rd.var else paste0(rd.var, '.rd')
        tmw <- fny(tme,cdv)
        tmt <- fny(tme,rdv)
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
    ## lsdfn adds to mancor the grouping labels of each block, taken from
    ## the block's own rows rather than parsed from the block names, whose
    ## level order depends on the column order of rd
    lsdfn <- function(mancor, blocks){
        revn <- cClass(rd, 'factor')
        rn <- do.call(rbind,mancor)
        codes. <- do.call(rbind, Map(function(m, b)
            b[rep(1L, nrow(m)), revn, drop = FALSE], mancor, blocks))
        rn <- cbind(rn, as.data.frame(codes.))
        rownames(rn) <- NULL
        return(rn)}
    ## adding levels in rd to tmp
    tmp <- lsdfn(tmp, tmp0)
    ## order data in tmp
    tmp <- ford(tmp,nm = 'lag')
    tmp <- groupedData(
        lmeForm(tmp,covar = 'lag'),data = tmp)
    md <- list(mmgram = tmp, call = sys.call())
    class(md) <- c('muleMan', class(md))
    return(md)
### \code{data.frame} object of multivariate correlations.
} , ex=function() {
    ## ring-width chronologies (mm) of eight cores and the allometric
    ## parameters of the modelFrame example
    data(Pchron, envir = environment())
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    ## tree-biomass fluctuations
    trwf <- Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   log.t = FALSE, on.time = FALSE)
    ## aridity-index fluctuations; hydrological years labeled by the year
    ## in which they end
    data(Prec, envir = environment())
    data(Temp, envir = environment())
    aif <- list(Prec, Temp) |>
        modelFrame(fn = list('moveYr', 'wlai'), lv = list('year', 'year'),
                   form = 'lmeForm')
    ## Mantel correlograms between the two, for each core (100
    ## permutations to keep the example short)
    set.seed(1)
    mcomp <- muleMan(trwf, aif, nperm = 100)
    head(mcomp$mmgram)
})
