amod <- structure(function#Allometric scaling.
### Allometric models and parameters are used to scale organic growth.
                         ##details<<. Allometric models are useful to
                         ##scale size-components of organisms such as
                         ##tree diameters (mp = \code{c(2,1)}) and
                         ##basal areas (mp = \code{c(0.25 *
                         ##pi,2)}). Several parameter groups
                         ##(\code{c(a1,b1,a2,b2, ..., an,bn)}) can be
                         ##recursively processed.  This enables
                         ##computation of complex organic
                         ##variables. For example, above-ground tree
                         ##biomass could be computed from two
                         ##parameter groups for tree-biomass, and
                         ##over-bark diameter scaling.
                         (
                             x, ##<<\code{numeric} vector.
                             mp = c(1,1), ##<<\code{numeric}. Allometric
                                          ##parameters. Default
                                          ##\code{c(1,1)} (see
                                          ##details).
                             fun = y ~ a*(x ^ b) ##<<\code{formula}.
                                                 ##Allometric
                                                 ##model. To properly
                                                 ##specify other
                                                 ##formulas, the
                                                 ##variables (e.g. x
                                                 ##and y) should
                                                 ##belong to
                                                 ##\code{letters[20:26]}.
                         ) {
                             
                             xn. <- FALSE
                             if(is.data.frame(x)){
                                 xnu <- cClass(x,'numeric')
                                 xn <- c(cClass(x,'integer'),
                                         cClass(x,'factor'))
                                 xn. <- length(xn)!=0
                                 xn.. <- xn[!xn%in%c('x','csx')]
                                 cd <- x
                                 x <- x[,'csx']
                                 names(x) <- cd[,'year']}
                             
                             
                             feval <- function(fun,...){
                                 e <- list(...)
                                 y <- eval(parse(text=fun), e)
                                 return(y)}
                             allv <- all.vars(fun)
                             prm <- allv[!allv%in%letters[20:26]]
                             spt <- ceiling(seq_along(mp)/length(prm))
                             if(!is.list(mp)){
                                 mp <- split(mp,spt)
                             }
                             dpr <- data.frame(do.call(rbind,mp))
                             names(dpr) <- prm
                             for(i in 1:length(mp)){
                                 x <- do.call(feval,
                                              c(fun,as.list(dpr[i,])))
                             }
                             
                             x1 <- c(NA,diff(x))
                             names(x1) <- names(x)
                             xd <- data.frame(x = x1, csx = x)
                             
                             if(xn.&& length(xnu) > 1){
                                 xd <- cd[,xnu]
                                 xd[,'x'] <- x1
                                 xd[,'csx'] <- x }
                             if(xn.)
                                 xd <- cbind(xd,cd[,xn..])
                                 
                                 return(xd)
### \code{data.frame} of the scaled variable (x) and relative
###  increments (csx). These are computed with \code{\link{setdiff}}
###  function.
                         } , ex=function() {
                             ## Simulating TRW records:
                             set.seed(1)
                             trw <- ts(abs(rnorm(12,1,1)),start = 1950)
                             ## Cumulative TRW:
                             cri <- cumsum(trw)
                             ## tree diameters
                             td <- amod(cri,mp = c(2,1))
                             ## plot of the tree diameters and the
                             ## relative increments:
                             plot(ts(td))
                         })
