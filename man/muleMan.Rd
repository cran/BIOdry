\name{muleMan}
\alias{muleMan}
\title{Multilevel dendroclimatic correlograms}
\description{Multivariate correlograms between TRW fluctuations and climatic
fluctuations.}
\usage{muleMan(rd, cd, rd.var = NULL, cd.var = NULL, ...)}
\arguments{
  \item{rd}{\code{dataframe} or \code{\link{groupedData}}. TRW
fluctuations such as that produced by
\code{\link{modelFrame}}.}
  \item{cd}{\code{dataframe} or
\code{\link{groupedData}}. Aridity-index fluctuations such
as that produced by \code{\link{modelFrame}}.}
  \item{rd.var}{\code{character} or \code{NULL}. Column name of
the TRW fluctuations to be compared. If
\code{NULL} then the first column is processed.}
  \item{cd.var}{\code{character} or \code{NULL}. Column name of
the aridity-index fluctuations to be compared. If
\code{NULL} then the first column is used.}
  \item{\dots}{Further arguments in \code{\link{mgram}}}
}
\details{Function \code{\link{mgram}} in
package \code{\link{ecodist}} is implemented to
compare the dendroclimatic fluctuations. Models
being compared should have common higher-level
factors (see example).}
\value{\code{data.frame} object of multivariate correlations.}
\references{Lara W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




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
## function)
trwf <- modelFrame(Pchron,
                   to = 'cm',
                   MoreArgs = list(mp = c(2,1, biom_param)),
                   log.t = FALSE,
                   on.time = FALSE)
## Climatic Records:
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
}
