mUnits <- structure(function#Metric system
### This function control metric units.
                    ##details<<Characters in \code{from} and \code{to}
                    ##arguments have the form 'p_', where 'p' is the
                    ##metric prefix and '_' is a base unit.  Sixteen
                    ##metric prefixes are supported: atto 'a', femto
                    ##'f', pico 'p', nano 'n', micro 'mm', mili 'm',
                    ##centi 'c', deci 'd', deca 'da', hecto 'h', kilo
                    ##'k', mega 'M', giga 'G', tera 'T', peta 'P', and
                    ##exa 'E'.
(
    
    x, ##<<\code{numeric} vector.
    from = 'mm', ##<<\code{character}. Initial metric unit.
    to = 'mm' ##<<\code{character}. Final metric unit.
) {
    fmu <- function(x){
        substr(x, nchar(x), nchar(x))}
    mu <- fmu(from);mu. <- fmu(to)
        err. <- paste('different metric units: ',
              mu,' vs. ',mu.,sep = '')
    if(!mu%in%fmu(to)) stop(err.)
    xp <- c(0:2,seq(3,18,3))
    ex <- c(-xp,xp)
    sm <- 10^(unique(ex[order(ex)]))
    us <- c('a','f','p','n','mm','m',
            'c','d','', 'da','h','k',
            'M','G','T','P','E')
    un <- paste(us,mu,sep ='')
    names(sm) <- un
    eq <- sm[from]/sm[to]
    names(eq) <- to
    x <- x * eq
    return(x)    
### \code{numeric} vector.
            
} , ex=function() {
    ## Simulation of TRW data
    set.seed(1)
    w <- abs(rnorm(12,1,1))
    trw <- ts(w,start = 1970)
    ## transforming metric units of trw vector from milimeters to meters
    sr <- mUnits(trw, from = 'mm', to = 'm')
    attributes(sr)
})
