\name{anova.modelFrame}
\alias{anova.modelFrame}
\title{Compare modelFrame objects}
\description{Models in \code{\link{modelFrame}} lists are compared with
\code{\link{anova.lme}} method.}
\usage{\method{anova}{modelFrame}(object, ..., test, type, adjustSigma, Terms, 
    L, verbose)}
\arguments{
  \item{object}{an object inheriting from class "modelFrame".}
  \item{\dots}{other optional fitted model objects inheriting from
classes "modelFrame", "lme", "lm", among other (see
\code{\link{anova.lme}}).}
  \item{test}{optional character string specifying the type of sum of
squares to be used in F-tests for the terms in the model
(see \code{\link{anova.lme}}).}
  \item{type}{optional character string specifying the type of sum
of squares to be used in F-tests for the terms in the
model (see \code{\link{anova.lme}}).}
  \item{adjustSigma}{If TRUE and the estimation method used to obtain
object was maximum likelihood, the residual
standard error is multiplied by sqrt(nobs/(nobs -
npar)), converting it to a REML-like estimate (see
\code{\link{anova.lme}}).}
  \item{Terms}{optional integer or character vector specifying which
terms in the model should be jointly tested to be zero
using a Wald F-test (see \code{\link{anova.lme}}).}
  \item{L}{optional numeric vector or array specifying linear
combinations of the coefficients in the model that should be
tested to be zero (see \code{\link{anova.lme}}).}
  \item{verbose}{optional logical value. If TRUE, the calling
sequences for each fitted model object are printed with
the rest of the output, being omitted if verbose =
FALSE (see \code{\link{anova.lme}}).}
}

\value{data frame inheriting from class "anova.lme".}
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
 \donttest{
 trwf <- modelFrame(Pchron,
                    to = 'cm',
                    MoreArgs = list(mp = c(2,1, biom_param)),
                    log.t = FALSE,
                    on.time = FALSE)
 }    
## Fitting a single linear regression of the "tdForm" formula
## without random effects to the tree-biomass data:
 \donttest{
 trwfl <- lm(log(x) ~ log(csx) + year,
             data = trwf$'model'$'data')
 }
## Comparing model likelihoods with anova method:
 \donttest{
     anova(trwf, trwfl)
 }
}
