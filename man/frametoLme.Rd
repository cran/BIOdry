\name{frametoLme}
\alias{frametoLme}
\title{LME modeling}
\description{LME models are fitted to detrend multilevel ecological data series.}
\usage{frametoLme(rd, form = "lmeForm", res.data = TRUE, ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{form}{\code{character}. Name of a lme method
. Available methods are 'lmeForm' and 'tdForm'
(see details).}
  \item{res.data}{\code{logical}. Save residuals as a
multilevel ecological data series. If TRUE then
a data frame of name 'resid' is added to output
list.}
  \item{\dots}{Further arguments to be passed to \code{\link{lme}} funtion or to
the lme method in \code{form}.}
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
\value{Depending on the \code{res.data} argument, either an LME model or a
\code{list} with both: the LME model and a MEDS of residuals}
\references{Pinheiro J. C.,
D. M. Bates. 2000. Mixed-effects models in S
and S-PLUS. Springer, New York.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## Monthly precipitation sums and average temperatures:
data(PTclim05,envir = environment())

##Modeling tree growth:
mpin <- modelFrame(Prings05,
                   y = Pradii03,
                   form = NULL,# on.time = TRUE,
                   MoreArgs = list(only.dup = TRUE,
                                   mp = c(1,1),
                                   un = c('mm','cm'),
                                   z = 2003))

## Detrending the tree-growth MEDS with a (l)td-form model:
rlme <- frametoLme(mpin,
                   form = 'tdForm',
                   method = 'ML',
                   log.t = TRUE)
summary(rlme$model)

##a plot of the modeled fluctuations
d <- groupedData(lmeForm(rlme$resid,lev.rm = 1),data = rlme$resid)
plot(d,groups = ~ sample,auto.key = TRUE)

## A model of aridity: 
cf <- modelFrame(PTclim05,
                 lv = list('year','year'),
                 fn = list('moveYr','wlai'),
                 form = NULL)
summary(cf)

## An lme model of aridity at 'plot' level:
rmod <- frametoLme(cf,form = 'lmeForm')
summary(rmod$model)

rk <- groupedData(lmeForm(rmod$resid),data=rmod$resid)
plot(rk,ylab = 'detrended AI')

}
