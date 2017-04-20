\name{tdForm}
\alias{tdForm}
\title{ltd formulas}
\description{This function formulates linear time-decline formulas (ltd) from
categorical variables in multilevel ecological data series.}
\usage{tdForm(rd, prim.cov = FALSE, on.time = TRUE, log.t = FALSE, lev.rm = NULL)}
\arguments{
  \item{rd}{\code{data.frame} or \code{character} vector. Multilevel
ecological data series or vector of ecological factors.}
  \item{prim.cov}{\code{logical}. Print a primary covariate
form: \code{'~ cov'}. If FALSE then a complete
formula: \code{'resp ~ cov | group'} is printed.}
  \item{on.time}{\code{logical}. If TRUE then \code{t =
'time'} (see \code{\link{rtimes}}). If FALSE
then \code{t = 'year'}.}
  \item{log.t}{\code{logical}. If TRUE then \code{f(time) =
ln(time)}. Default FALSE produces a log-linear
time-decline formula.}
  \item{lev.rm}{NULL or \code{character} name of the ecological
factor(s) in the MEDS to be removed from the
formula.}
}
\details{the ltd formulas belong to following
general equation: log (x) = log (csx) + f(time);
where the relative organic growth (x) is
explained by the cumulative organic growth (csx)
plus a function of time f(time); with f(time)
being either the time or a logarithmic
transformation the time. The ltd can be
implemented by \code{\link{modelFrame}} function
to subtract trends in organic MEDS}
\value{\code{formula} with the forms: 'resp ~ cov | group' or '~ cov'.}
\references{Zeide B. 1993. Analysis of Growth
Equations. For. Sci., 39: 594-616.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## an ltd formula:
lev <- c('plot','tree')
tdeq <- tdForm(lev,log.t = TRUE)
tdeq
## (not run) only primary covariate:
tdeq1 <- tdForm(lev,prim.cov = TRUE)
tdeq1
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## removing two levels: 'plot' and 'tree' from the formula
tdea2 <- tdForm(Prings05, lev.rm = c('plot','tree'))
tdea2 <- tdForm(Prings05, lev.rm = 2:3)
}
