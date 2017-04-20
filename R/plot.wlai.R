plot.wlai <- structure(function#Plot an wlai object
### A Walter-Lieth climate diagram is produced.

                       ##details<< Areas between temperature and
                       ##precipitation lines when precipitation
                       ##exceeds temperature (moist seasons) are
                       ##plotted in gray color, and areas where
                       ##temperature exceeds precipitation (dry
                       ##seasons) are plotted in black color. Monthly
                       ##cumulative precipitations over 100 mm are
                       ##scaled such that 1 degree C of average
                       ##temperature is equal to 5 mm of
                       ##precipitation.

                  ##references<< Manrique E.,
                  ##A. Fernandez-Cancio. 2000. Extreme climatic events
                  ##in dendroclimatic reconstructions from
                  ##Spain. Clim. Chang., 44: 123-138.
(
    x,##<< \code{vector} or \code{data.frame}. An object inheriting
    ##from class \code{\link{wlai}}, representing the Aridity Index.
    ... ##<<\code{logical}. Further arguments passed to
         ##\code{\link{plot}} function.
)
{
    if(!inherits(x, 'wlai'))
        stop("'x' does not belong to class 'wlai'")
    pl <- attributes(x)
    fb <- function(x){
        nx <- c(pl$'ptm',x,pl$'ptm')
        y <- as.numeric(names(x))
        names(nx) <- c(min(y),names(x),max(y))
        nxn <- as.numeric(names(nx)) - 0.5
        dt <- data.frame(x = nxn, y = nx)
        return(dt)}
    ## return(fb(pl$'xn'))
    ##color palette
    col. <- paste('gray',c(70,60,30), sep = '')
    par(oma = c(0,0,0,2))
    plot(fb(pl$'xn'),
         ylim = c(pl$'ptm',max(c(pl$'pr.',pl$'tm'))),
         col = col.[1],type='l',
         xaxt = 'n',yaxt = 'n',
         xlab = 'Month',ylab = NA, ...)
    mns <- rownames(pl$'cd')
    xap <- unique(fb(pl$'pr')[,1])
    yap <- pretty(c(pl$'ptm',max(c(pl$'pr.',pl$'tm'))))
    axis(4,at = yap, labels = yap,las = 1)
    axis(2,at = yap, labels = yap/2,las = 1)
    axis(1,at = xap, tick = FALSE,labels = mns)            
    axis(1,at = xap - 0.5, tick = TRUE,labels = FALSE)            
    mtext(text = expression(~degree~C),
          las = 1,at = min(xap) - 1.2 * par('cex'))
    mtext(text = 'mm',las = 1,
          at = max(xap) + 1.2 *par('cex'))
    polygon(fb(pl$'xn'), border = NA, col = col.[2])
    polygon(fb(pl$'xn..'), border = NA, col = col.[1])
    polygon(fb(pl$'tm'), border = NA, col = col.[3])
    polygon(fb(pl$'xn1'), border = col.[1], col = 'white')
    lines(c(min(xap),max(xap)),c(100,100),col =col.[1],lty = 1)
    lp <- 'top'
    legend(lp,legend = c('Dry season','Moist season'),
           fill = c(col.[3],col.[1]),cex = 0.8,
           horiz = TRUE, border = NA,bty = 'n')
    rai <- round(pl$'ai', 3)
    text(1.5,
         max(c(pl$'pr.',pl$'tm')),
         cex = 0.8,
         col = 'gray30',
         paste("AI = ", rai, sep = ''))
### A \code{\link{plot}} of the Walter-Lieth diagram.
} ,
ex=function() {
    ##random simulation of climatic records
    set.seed(1)
    pr <- rnorm(12,1,1)
    tm <- rnorm(12,0,1)
    cld <- data.frame(pr,tm)
    ##labels of months from october to september
    rownames(cld) <- month.abb[c(10:12,1:9)]
    rownames(cld) <- c(10:12,1:9)
    ##computation of the aridity index and climate diagram
    AI <- wlai(cld)
    plot.wlai(AI)
})
