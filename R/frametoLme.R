frametoLme <- structure(function# LME modeling
### LME models are fitted to detrend multilevel ecological data series.
                        ##details<<This function implements
                        ##\code{\link{lme}} function to fit linear
                        ##mixed-effects models on multilevel
                        ##ecological data series processed by the
                        ##\code{\link{modelFrame}} function. Two kind
                        ##of model formulas can be fitted: 'lmeForm'
                        ##and 'tdForm'; these characters implement
                        ##functions with same names
                        ##(\code{\link{tdForm}} and
                        ##\code{\link{lmeForm}}). Other lme formulas
                        ##can be specified by modifying arguments in
                        ##any of these two functions. After the lme
                        ##models are fitted, they can be extended by
                        ##implementing methods in \code{\link{nlme}}
                        ##package.
                        
                        ##references<< Pinheiro J. C.,
                        ##D. M. Bates. 2000. Mixed-effects models in S
                        ##and S-PLUS. Springer, New York.
(
    rd, ##<<\code{data.frame}. Multilevel ecological data series.
    form = 'lmeForm', ##<<\code{character}. Any of two lme formulas:
                      ##'lmeForm' and 'tdForm' (see details).
    res.data =TRUE, ##<< \code{logical}. Save residuals as a
                    ##multilevel ecological data series. If TRUE then
                    ##a data frame of name 'fluc' is added to output
                    ##list.
    ... ##<< Further arguments to be passed to \code{\link{lme}}
        ##function or to the lme formula in \code{form}.
) {
    pr.cov <- function(form){
        chf <- Reduce(paste,deparse(form))
        fnc <- gsub('.*~|\\|.*','',chf)
        fnc <- paste('~',fnc,sep = '')
        return(formula(fnc))}
    ## Implementation of lme form:
    if(grepl('~',form))
        formu <- formula(form)
    if(!grepl('~',form)){
        arf <- arguSelect(rd,fun = form,...)
        formu <- do.call(form,arf)}
    prc <- pr.cov(formu)
    environment(prc) <- .GlobalEnv
    gd <- groupedData(
        formula = formu,data = na.omit(rd))  #<<
    arl <- arguSelect(NULL,fun = 'lme',...)
    arl[['fixed']] <- gd
    if(!'random'%in%names(arl))
        arl[['random']] <- pdDiag(prc)
    if(!'control'%in%names(arl))
        arl[['control']] <- list(msMaxIter = 200)
    argn <- lapply(names(arl), as.name)
    names(argn) <- names(arl)
    call <- as.call(c(list(as.name("lme")), argn))
    mem. <- eval(call, arl)
    mem <- list(model = mem.,call = sys.call())
    rset <- function(r.model){
        md <- r.model[['data']]
        tim <- cClass(md, 'integer')
        lev <- cClass(md, 'factor')
        lg <- ncol(data.frame(getGroups(md)))
        dcum <- residuals(r.model,level = lg:1,type = 'p')
        dcum <- as.data.frame(dcum)
        nam. <- names(dcum)
        names(dcum) <- paste(nam.,'.res',sep = '')
        ## dres <- cbind(dcum,md[,c(tim,lev)])
        dres <- merge(dcum,md[,c(tim,lev)],
                      by = 'row.names', all.x = TRUE,sort = FALSE)
        dres <- dres[,c(names(dcum),tim,lev)]
        return(dres)}
    if(res.data){
            residu <- rset(mem.)
        mem[['fluc']] <- groupedData(lmeForm(residu),data=residu)
        }
            ## mem[['resid']] <- rset(mem.)
    return(mem)
### \code{\link{groupedData}} object.
} , ex=function() {
    
    ##TRW chronology (mm) and inside-bark radii
    data(Pchron,envir = environment())
    
    ## Parameters of allometric model to compute Diameter at Breast
    ## Height over bark (DBH, cm) from diameter inside bark (dib, cm)
    ## and Total Tree Biomass (TTB, kg tree -1 ) from DBH (Lara
    ## et. al. 2013):
    biom_param <- c(2.87, 0.85, 0.05, 2.5)

    ## Modeling tree-biomass fluctuations while accounting for
    ## within-plot source variability (see defaults in "modelFrame"
    ## function)
    ## \donttest{
    ## trwf <- modelFrame(Pchron,
    ##                    to = 'cm',
    ##                    MoreArgs = list(mp = c(2,1, biom_param)),
    ##                    log.t = FALSE,
    ##                    on.time = FALSE)
    ## }
    ## Detrending the fluctuations by fitting a (l)td-form model
    ## with Maximum-likelihood method (ML):
    ## \donttest{
    ## pdata <- trwf$'model'$'data'
    ## rlme <- frametoLme(pdata,
    ##                    form = 'tdForm',
    ##                    method = 'ML',
    ##                    log.t = TRUE)
    ## summary(rlme$model)
    ## }
    ##a plot of the modeled fluctuations:
    ## \donttest{
    ## d <- groupedData(lmeForm(rlme$fluc,lev.rm = 1),data = rlme$fluc)
    ## plot(d,groups = ~ sample,auto.key = TRUE)
    ## }
    ## A model of aridity:
    ## \donttest{
    ## cf <- modelFrame(PTclim05,
    ##                  lv = list('year','year'),
    ##                  fn = list('moveYr','wlai'),
    ##                  form = NULL)
    ## summary(cf)
    ## }
    ## An lme model of aridity at 'plot' level:
    ## \donttest{
    ## cdata <- cf$'model'$'data'
    ## rmod <- frametoLme(cdata,form = 'lmeForm')
    ## summary(rmod$model)
    ## rk <- groupedData(lmeForm(rmod$fluc),data=rmod$fluc)
    ## plot(rk,ylab = 'detrended AI')
    ## }
})
