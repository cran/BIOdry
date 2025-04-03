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



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
##TRW chronology (mm) and inside-bark radii
data(Pchron,envir = environment())

## Parameters of allometric model to compute Diameter at Breast
## Height over bark (DBH, cm) from diameter inside bark (dib, cm)
## and Total Tree Biomass (TTB, kg tree -1 ) from DBH (Lara
## et. al. 2013):
biom_param <- c(2.87, 0.85, 0.05, 2.5)

## Modeling tree-biomass fluctuations while accounting for
## within-plot source variability (see defaults in "modelFrame"
## function):
## \\donttest{
## trwf <- modelFrame(Pchron,
##                    to = 'cm',
##                    MoreArgs = list(mp = c(2,1, biom_param)),
##                    log.t = FALSE,
##                    on.time = FALSE)
## }
## Climatic Records:
data(Temp,envir = environment())
data(Prec,envir = environment())
## Aridity-index fluctuations:
## \\donttest{
##     aif <- modelFrame(rd = list(Prec, Temp),
##                       fn = list('moveYr','wlai'),
##                       lv = list('year','year'),
##                       form = 'lmeForm')
## }
##Multivariate comparison:
## \\donttest{
## mcomp <- muleMan(trwf,
##                     aif,
##                  nperm = 10^3)

## plot(mcomp, grid = FALSE)
## }
}
