\name{muleMan}
\alias{muleMan}
\title{Multilevel correlograms}
\description{Multilevel Mantel correlograms between two \code{\link{modelFrame}} objects.}
\usage{muleMan(rd, cd, rd.var = NULL, cd.var = NULL, plot.man = TRUE, 
    p.sig = 0.05, ...)}
\arguments{
  \item{rd}{\code{list} or \code{dataframe}, such as that produced
by \code{\link{modelFrame}}, containing the modeled tree
growth.}
  \item{cd}{\code{list} or \code{dataframe}, such as that produced
by \code{\link{modelFrame}}, with correspondatn modeled
aridity (see details).}
  \item{rd.var}{\code{character}. Column name of the
processed variable in code{rd}. If \code{NULL} then first column
in \code{rd} is processed.}
  \item{cd.var}{\code{character}. Column name of the
processed variable in \code{cd}. If
\code{NULL} then its first column is used.}
  \item{plot.man}{\code{Logical}. Plot the multi-level
correlogram. If TRUE then a plot at the common level is printed.}
  \item{p.sig}{\code{Numeric}. Threshold of significance in the
plot.}
  \item{\dots}{Further arguments in \code{\link{mgram}}}
}
\details{Function \code{\link{mgram}} in package \code{\link{ecodist}}  is implemented on two \code{\link{modelFrame}} objects, with the first object containing modeled tree growth, and the second one being the modeled aridity. Correspondant aridity model should have at least one level in common with the modeled growth (see example).}
\value{list with computed correlations}
\references{Lara W., F. Bravo, D. Maguire. 2013. Modeling patterns between drought and tree biomass growth from dendrochronological data: A multilevel approach. Agric. For. Meteorol., 178-179:140-151.                                            }
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
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
}
