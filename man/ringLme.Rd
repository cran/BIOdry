\name{ringLme}
\alias{ringLme}
\title{LME modeling}
\description{LME modeling of multilevel data frames. }
\usage{ringLme(rd, form = "lmeForm", var.mod = FALSE, arma.mod = FALSE, 
    res.data = TRUE, ...)}
\arguments{
  \item{rd}{\code{data.frame} object containing factor-level columns.}
  \item{form}{\code{character}. Method name of LME
formula. Available methods are 'lmeForm' and
'tdForm' (see details).}
  \item{var.mod}{\code{logical}. If TRUE then the fitted
model is extended by modeling residual
heteroscedaticity with 'varConstPower' class in
\code{\link{nlme}}.}
  \item{arma.mod}{\code{logical}. If TRUE then the fitted
model is extended by modeling residual
autocorrelation with class 'corARMA' in
\code{\link{nlme}}, with p = 1 and q = 1,
which models residual autocorrelation for lags
> 2.}
  \item{res.data}{\code{logical}. Save residuals as a
multilevel data frame. If TRUE then a data frame
of name 'resid' is added to output list}
  \item{\dots}{Further arguments to be passed to function in \code{form}.}
}
\details{This function implements \code{\link{lme}} to fit linear mixed-effects models on data frames with factor-level columns. Two kind of model formulas are available: 'lmeForm' and 'tdForm', these characters implement functions with same names (\code{\link{tdForm}} and \code{\link{lmeForm}}). Users can develop their own LME formulas by modifying arguments in any of these methods. After LME models are fitted, they  can be extended by modeling heteroscedasticity and autocorrelation of the residuals. Nevertheless, such residual modeling would take long time depending on the complexity  of the modeled levels.}
\value{Depending on \code{res.data}, either an LME model, or a
\code{list} with both: the LME model, and a residual data frame
containing initial factor-level columns.}
\references{Pinheiro J. C., D. M. Bates. 2000. Mixed-effects models in S and S-PLUS. Springer, New York.                                          }
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## Monthly precipitation sums and average temperatures:
data(PTclim05,envir = environment())

##Modeling tree growth:
mpin <- modelFrame(Prings05, y = Pradii03,
        form = NULL,# on.time = TRUE,
        MoreArgs = list(only.dup = TRUE,
        mp = c(1,1),un = c('mm','cm'),z = 2003))

## Detrending tree growth with a td-form model:
rlme <- ringLme(mpin,form = 'tdForm')
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
rmod <- ringLme(cf,form = 'lmeForm')
summary(rmod$model)

rk <- groupedData(lmeForm(rmod$resid),data=rmod$resid)
plot(rk,ylab = 'detrended AI')


}
