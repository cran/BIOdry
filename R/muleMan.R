muleMan <- structure(function#Multilevel correlograms
### Multilevel Mantel correlograms between two \code{\link{modelFrame}} objects.
##details<< Function \code{\link{mgram}} in package \code{\link{ecodist}}  is implemented on two \code{\link{modelFrame}} objects, with the first object containing modeled tree growth, and the second one being the modeled aridity. Correspondant aridity model should have at least one level in common with the modeled growth (see example).
##references<< Lara W., F. Bravo, D. Maguire. 2013. Modeling patterns between drought and tree biomass growth from dendrochronological data: A multilevel approach. Agric. For. Meteorol., 178-179:140-151.                                            
(
    rd, ##<<\code{list} or \code{dataframe}, such as that produced
    ##by \code{\link{modelFrame}}, containing the modeled tree
    ##growth.
    cd, ##<<\code{list} or \code{dataframe}, such as that produced
    ##by \code{\link{modelFrame}}, with correspondatn modeled
    ##aridity (see details).
    rd.var = NULL, ##<<\code{character}. Column name of the
    ##processed variable in code{rd}. If \code{NULL} then first column
    ##in \code{rd} is processed.
    cd.var = NULL, ##<<\code{character}. Column name of the
    ##processed variable in \code{cd}. If
    ##\code{NULL} then its first column is used.
    plot.man = TRUE, ##<<\code{Logical}. Plot the multi-level
    ##correlogram. If TRUE then a plot at the common level is printed.
    p.sig = 0.05, ##<<\code{Numeric}. Threshold of significance in the
    ##plot.
    ... ##<<Further arguments in \code{\link{mgram}}
) {
    
    if(!is.data.frame(rd))
        rd <- rd[['resid']]
    if(!is.data.frame(cd))
        cd <- cd[['resid']]
    if(is.null(rd.var))
        rd.var <- names(rd)[1]
    if(is.null(cd.var))
        cd.var <- names(cd)[1]
    
    ford <- function(cd,nm = 'year'){
        cd[do.call(order,as.list(cd[,
           rev(c(nm,colclass(cd,T)$'fac'))])),]}
    
    ## if(is.null(lv))
    lv <- colclass(rd,T)$'fac'[1]
    ## return(lv)
    tmp0 <- splitFrame(rd,lv)
    ni <- names(cd)%in%names(rd)
    nin <- names(cd)[ni]

    fm <- function(x,...){
        tme <- merge(x,cd,by = nin)
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
    
    ## lsdfn extracts levels from list names 
    lsdfn <- function(mancor,rd){
        rn <- do.call(rbind,mancor)
        code. <- rownames(rn)
        revn <- colclass(rd,TRUE)
        codes. <- do.call(rbind,strsplit(
                                    code.,'\\.'))[,1:length(revn[['fac']])]
        codes. <- data.frame(codes.)
        codes. <- codes.[,rev(names(codes.))]
        codes. <- lapply(codes.,as.factor)
        names(codes.) <- revn[['fac']]
        rn <- cbind(rn,codes.)
        rownames(rn) <- NULL
        return(rn)}
    ## adding levels in rd to tmp
    tmp <- lsdfn(tmp,rd)    
    ## order data in tmp
    tmp <- ford(tmp,nm = 'lag')
    fplot <- function(tmp,nin.,p.sig = 0.05,...){
        ns <- paste('p >',p.sig,sep = ' ')
        ys <- paste('p <',p.sig,sep = ' ')
        fr. <- 'mantelr ~ lag |'
        form <- formula(paste(fr.,'plot',sep = ''))
        tmp <- groupedData(form,data = tmp)     
        tmp[,'sig'] <- factor(with(tmp,
                                   ifelse(pval < p.sig,ys,ns)))
        ## trellis.device(color=FALSE)
        plot(tmp,groups = ~ sig,auto.key = TRUE,ylab = 'Mantel r',...)}
    
    if(plot.man){
        pl. <- fplot(tmp,p.sig,...)
        print(pl.)}
    
    return(tmp)
### list with computed correlations
} , ex=function() {
    ## Tree growh and aridity are modeled, and both models are
    ## correlated.
    
    ##Multilevel data frame of tree-ring widths:
    data(Prings05,envir = environment())
    ## Radial increments measured on 2003:
    data(Pradii03,envir = environment())    
    ## Monthly precipitations and temperatures:
    data(PTclim05,envir = environment())

    ## Modeled aridity
    cf <- modelFrame(rd=PTclim05,
                     lv = list('year','year'),
                     fn = list('moveYr','wlai'),
                     form = 'lmeForm')
    head(cf$resid)
    summary(cf$model)
    
    ## Modeled tree growth
    ar <- modelFrame(Prings05, y = Pradii03,
                     form = 'tdForm', on.time = TRUE,
                     MoreArgs = list(only.dup = TRUE,
                                     mp = c(1,1),un = c('mm','cm'),z = 2003))
    head(ar$resid)
    summary(ar$model)
    
    ## Multi-level correlogram
    mancor <- muleMan(ar,cf,nperm = 10^3)
    head(mancor)
})
