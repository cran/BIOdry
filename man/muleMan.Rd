\name{muleMan}
\alias{muleMan}
\title{Multilevel correlograms}
\description{Mantel correlograms between two multilevel ecological data series
(MEDS).}
\usage{muleMan(rd, cd, rd.var = NULL, cd.var = NULL, ...)}
\arguments{
  \item{rd}{\code{list} or \code{dataframe}. First multilevel ecological
data series, or \code{\link{modelFrame}} object, to be compared.}
  \item{cd}{\code{list} or \code{dataframe}. Second MEDS, or
\code{\link{modelFrame}} object, to be compared.}
  \item{rd.var}{\code{character} or \code{NULL}. Column name of
first MEDS in \code{rd} argument. If \code{NULL}
then the first column is processed.}
  \item{cd.var}{\code{character} or \code{NULL}. Column name of
second MEDS in \code{cd} argument. If \code{NULL}
then the first column is used.}
  \item{\dots}{Further arguments in \code{\link{mgram}}}
}
\details{Function \code{\link{mgram}} in
package \code{\link{ecodist}} is implemented to
compare two \code{\link{modelFrame}} objects,
with the first object containing modeled
fluctuations of tree growth, and the second one
being the modeled fluctuations of
aridity. Correspondent aridity model should
have at least one level in common with the
modeled tree growth (see example).}
\value{Multilevel ecological data series (\code{\link{groupedData}} object) with the 
Mantel correlations.}
\references{Lara W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## Fluctuations of tree growh and aridity are modeled and
## compared.

##Multilevel ecological data series of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## Monthly precipitations and temperatures:
data(PTclim05,envir = environment())

## Modeling fluctuations of aridity 
cf <- modelFrame(rd=PTclim05,
                 lv = list('year','year'),
                 fn = list('moveYr','wlai'),
                 form = 'lmeForm')
head(cf$resid)
summary(cf$model)

## Modeling fluctuations of tree growth
ar <- modelFrame(Prings05, y = Pradii03,
                 form = 'tdForm', on.time = TRUE,
                 MoreArgs = list(only.dup = TRUE,
                                 mp = c(1,1),un = c('mm','cm'),z = 2003))
head(ar$resid)
summary(ar$model)

## Multilevel correlogram:
mancor <- muleMan(ar,cf,nperm = 10^3)
head(mancor)

## Vector of significances (p < 0.05): 
sig <- with(mancor,ifelse(pval < 0.05,TRUE,FALSE))
## Plotting the multilevel correlograms with correspondent
## significances:
plot(mancor,
     groups = sig,
     pch = c(21,19),
     grid = FALSE,
     abline = list(h = 0, lty = 2, lwd = 0.5),
     layout = c(4,2))

}
