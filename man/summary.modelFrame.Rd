\name{summary.modelFrame}
\alias{summary.modelFrame}
\title{summarize a modelFrame object}
\description{A summary of a \code{\link{modelFrame}} \code{object} is obtained.}
\usage{\method{summary}{modelFrame}(object, 
    ...)}
\arguments{
  \item{object}{an \code{object} inheriting from class
\code{\link{modelFrame}}.}
  \item{\dots}{additional optional arguments passed to
\code{\link[nlme]{summary.lme}} method.}
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




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
## function):
## \\donttest{
## trwf <- modelFrame(Pchron,
##                    to = 'cm',
##                    MoreArgs = list(mp = c(2,1, biom_param)),
##                    log.t = TRUE,
##                    on.time = TRUE)
## summary(trwf)
## }
}
