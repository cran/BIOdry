\name{plot.muleMan}
\alias{plot.muleMan}
\title{Plot muleMan objects}
\description{Diagnostic Trellis plot for fluctuations in
\code{\link{muleMan}} objects are obtained.}
\usage{\method{plot}{muleMan}(x, ...)}
\arguments{
  \item{x}{An object inheriting from class \code{\link{muleMan}}.}
  \item{\dots}{further arguments passed to the Trellis plot function.}
}



\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
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

plot(mcomp, grid = FALSE)    
}
