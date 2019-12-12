\name{frametoLme}
\alias{frametoLme}
\title{LME modeling}
\description{LME models are fitted to detrend multilevel ecological data series.}
\usage{frametoLme(rd, form = "lmeForm", res.data = TRUE, ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{form}{\code{character}. Any of two lme formulas:
'lmeForm' and 'tdForm' (see details).}
  \item{res.data}{\code{logical}. Save residuals as a
multilevel ecological data series. If TRUE then
a data frame of name 'fluc' is added to output
list.}
  \item{\dots}{Further arguments to be passed to \code{\link{lme}}
function or to the lme formula in \code{form}.}
}
\details{This function implements
\code{\link{lme}} function to fit linear
mixed-effects models on multilevel
ecological data series processed by the
\code{\link{modelFrame}} function. Two kind
of model formulas can be fitted: 'lmeForm'
and 'tdForm'; these characters implement
functions with same names
(\code{\link{tdForm}} and
\code{\link{lmeForm}}). Other lme formulas
can be specified by modifying arguments in
any of these two functions. After the lme
models are fitted, they can be extended by
implementing methods in \code{\link{nlme}}
package.}
\value{\code{\link{groupedData}} object.}
\references{Pinheiro J. C.,
D. M. Bates. 2000. Mixed-effects models in S
and S-PLUS. Springer, New York.}
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
 \donttest{
 trwf <- modelFrame(Pchron,
                    to = 'cm',
                    MoreArgs = list(mp = c(2,1, biom_param)),
                    log.t = FALSE,
                    on.time = FALSE)
 }
## Detrending the fluctuations by fitting a (l)td-form model
## with Maximum-likelihood method (ML):
 \donttest{
 pdata <- trwf$'model'$'data'
 rlme <- frametoLme(pdata,
                    form = 'tdForm',
                    method = 'ML',
                    log.t = TRUE)
 summary(rlme$model)
 }
##a plot of the modeled fluctuations:
 \donttest{
 d <- groupedData(lmeForm(rlme$fluc,lev.rm = 1),data = rlme$fluc)
 plot(d,groups = ~ sample,auto.key = TRUE)
 }
## A model of aridity:
 \donttest{
 cf <- modelFrame(PTclim05,
                  lv = list('year','year'),
                  fn = list('moveYr','wlai'),
                  form = NULL)
 summary(cf)
 }
## An lme model of aridity at 'plot' level:
 \donttest{
 cdata <- cf$'model'$'data'
 rmod <- frametoLme(cdata,form = 'lmeForm')
 summary(rmod$model)
 rk <- groupedData(lmeForm(rmod$fluc),data=rmod$fluc)
 plot(rk,ylab = 'detrended AI')
 }
}
