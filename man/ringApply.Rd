\name{ringApply}
\alias{ringApply}
\title{Multilevel apply}
\description{Wrapper of \code{\link{Map}} to apply functions on multilevel data
series and preserve factor-level structure in the outputs.}
\usage{ringApply(rd, lv = 1, fn = "scacum", ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multileve ecological data series.}
  \item{lv}{{numeric} position, or {character} name, of an
ecological factor in the processed MEDS.}
  \item{fn}{\code{character} name of the function to be
evaluated (see details). Default 'scacum'
computes scaled-cummulative radii.}
  \item{\dots}{Further arguments in the function being specified
\code{fn} argument (see details)}
}
\details{Other functions such as
\code{\link{rtimes}}, \code{\link{scacum}},
\code{\link{amod}}, or \code{\link{wlai}} can
be implemented.  Function arguments should be
formulated as suggested in
\code{\link{mapply}}, with no vectorized
arguments being stored in a \code{MoreArgs}
list. This function is implemented by
\code{\link{modelFrame}} for recursive
modeling of MEDS.}
\value{\code{data.frame} object preserving initial factor-level columns.}
\references{Lara, W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel ecological data series (MEDS) of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## MEDS of monthly precipitation sums and average temperatures:
data(PTclim05,envir = environment())

##Time units in Prings05 object are sinchronized at tree level
##with 'rtimes' function:
dfm1 <- ringApply(Prings05,lv = 2,fn = 'rtimes')
str(dfm1)
##time units from time 1 to time 9:
subset(dfm1,time\%in\%c(1:9,NA))

## Cummulative radial increments are processed at 'sample' level:
dfm2 <- ringApply(dfm1,lv = 'sample',y = Pradii03,fn = 'scacum')
str(dfm2)    
##Allometric modeling at 'sample' level:
dfm3 <- ringApply(dfm2,lv = 'sample',fn = 'amod',
                  MoreArgs = list(mp = c(1,1,0.25 * pi,2),
                                  un = c('mm','m')))
str(dfm3)

## seasonal years from 'October' to 'September':
cl1 <- ringApply(PTclim05,lv = 'year',fn = 'moveYr')
tail(cl1,15)
##using ringApply() function to compute multilevel aridity indexes 
##('wlai' function) at 'year' level:
wl <- ringApply(cl1,lv = 'year',fn = 'wlai')
str(wl)#only time units with 12 months are evaluated 

## A plot of the modeled fluctuations of aridity
d <- groupedData(lmeForm(wl),wl)
plot(d)        

}
