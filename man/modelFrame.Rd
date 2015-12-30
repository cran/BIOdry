\name{modelFrame}
\alias{modelFrame}
\title{Multilevel modeling}
\description{Multilevel modeling of data frames with factor-level columns.}
\usage{modelFrame(rd, lv = list(2, 1, 1), fn = list("rtimes", "scacum", 
    "amod"), form = NULL, ...)}
\arguments{
  \item{rd}{\code{data.frame} object with factor-level columns.}
  \item{lv}{List of \code{numeric} positions in the
factor-level columns to implement the
functions, or \code{character} names of such
columns.}
  \item{fn}{List of \code{character}
names of the functions to
be implemented (see
details).}
  \item{form}{\code{character} or \code{NULL}. Name of an LME
formula to be used in final residual modeling.  Two
methods are available: 'lmeForm' or 'tdForm'. These
methods implement functions with the same names (see
\code{\link{lmeForm}} and \code{\link{tdForm}}). If
\code{NULL} then such a modeling is not developed.}
  \item{\dots}{Combined arguments to be evaluated by functions in
\code{fn} (see details).}
}
\details{Default arguments produce modeling of tree growth from dendrochonological data with recursive implementation of \code{\link{rtimes}}, \code{\link{scacum}}, \code{\link{amod}}, and \code{\link{ringLme}}. Nevertheless, other functions can also be implemented (see example with climatic data). Internal algorithm uses \code{\link{arguSelect}} and \code{\link{ringApply}}. Consequently, arguments not to be vectorized should be specified in a 'MoreArgs' list (see example).}
\value{Depending on \code{form}, either data frame, or fitted lme
object (see \code{\link{nlmeObject}}) model}
\references{Lara W., F. Bravo, D. Maguire. 2013. Modeling patterns between drought and tree biomass growth from dendrochronological data: A multilevel approach. Agric. For. Meteorol., 178-179:140-151.                                                                  }
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## Climatic records of monthly precipitations and temperatures
data(PTclim05,envir = environment())

## Modeling tree growth:
ar <- modelFrame(Prings05, y = Pradii03,form = 'tdForm',
                 MoreArgs = list(only.dup = TRUE,
                 mp = c(1,1),un = c('mm','cm'),z = 2003))
head(ar$resid)
summary(ar$model)    
##a plot of the tree-growth fluctuations:
d <- groupedData(lmeForm(ar$resid,lev.rm = 1),data = ar$resid)
plot(d,groups = ~ sample,auto.key = TRUE)

## Modeling aridity:
cf <- modelFrame(rd=PTclim05,
     lv = list('year','year'),
     fn = list('moveYr','wlai'),
     form = 'lmeForm')
head(cf$resid)
##a plot of the aridity fluctuations:
dc <- groupedData(lmeForm(cf$resid),data = cf$resid)
plot(dc, auto.key = TRUE)

}
