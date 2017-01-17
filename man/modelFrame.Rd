\name{modelFrame}
\alias{modelFrame}
\title{MEDS modeling }
\description{This function develops recursive modeling and/or LME detrending of
multilevel ecological data series (MEDS).}
\usage{modelFrame(rd, lv = list(2, 1, 1), fn = list("rtimes", "scacum", 
    "amod"), form = NULL, ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{lv}{List of \code{numeric} positions in the
factor-level columns of the processed MEDS to
implement the one-level functions, or
\code{character} names of the factor-level
columns.}
  \item{fn}{List of \code{character}
names of the one-level
functions to be recursively
implemented (see details).}
  \item{form}{\code{character} or \code{NULL}. Name of an LME
formula to be used in residual subtraction.  Two
methods are available: 'lmeForm' or 'tdForm'. These
methods implement functions with the same names
(see \code{\link{lmeForm}} and
\code{\link{tdForm}}). If \code{NULL} then the lme
modeling is not developed.}
  \item{\dots}{Further arguments to be evaluated by one-level or
\code{\link{lme}} functions (see details).}
}
\details{Diverse one-level functions are
recursively implemented. Processed MEDS can
also be detrended with \code{\link{lme}}
models and other \code{\link{nlme}}
methods. Default arguments produce modeling
of tree growth from dendrochonological MEDS
with recursive implementation of
\code{\link{rtimes}}, \code{\link{scacum}},
\code{\link{amod}}, and
\code{\link{frametoLme}}
functions. Nevertheless, other functions can
be implemented (see example with climatic
data). Internal algorithm uses
\code{\link{arguSelect}} and
\code{\link{ringApply}}
functions. Consequently, arguments not to be
vectorized should be specified in 'MoreArgs'
lists (see example). Objects of type
\code{modelFrame} can further be modified
with \code{\link{update}} function.}
\value{Depending on the \code{form} argument, either MEDS or
\code{\link{lme}} object.}
\references{Lara W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## Climatic records of monthly precipitations and temperatures
data(PTclim05,envir = environment())

## Modeling tree growth:
ar <- modelFrame(Prings05,
                 y = Pradii03,
                 form = 'tdForm',
                 MoreArgs = list(only.dup = TRUE,
                                 mp = c(1,1),
                                 un = c('mm','cm'),z = 2003))
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
