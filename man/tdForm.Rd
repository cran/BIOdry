\name{tdForm}
\alias{tdForm}
\title{Time-decline formula}
\description{LME formulas of type time-decline (td) or log-time-decline (ltd).}
\usage{tdForm(rd, prim.cov = FALSE, on.time = TRUE, log.t = FALSE, lev.rm = NULL)}
\arguments{
  \item{rd}{\code{data.frame} object with factor-level columns, or
\code{character} vector with names of factor-level columns
which are decreasingly ordered.}
  \item{prim.cov}{\code{logical}. Print a primary covariate form:
'~ cov'. If FALSE then a complete
formula: 'resp ~ cov | group' is printed.}
  \item{on.time}{\code{logical}. If TRUE then t =
'time' (see \code{\link{rtimes}}). If FALSE then
t = 'year'.}
  \item{log.t}{\code{logical}. If TRUE then \code{f(t) =
ln(t)}. Default FALSE produces a td form}
  \item{lev.rm}{NULL or \code{character} name of the
factor-level column to be removed from the
formula.}
}
\details{(L)td formulas belongs to general equation: log (cs) = log (x) + f(t); where the cummulative radial increments (cs) are explained by the observed radial increments (x) plus a function of time f(t); with f(t) being either the time or a logarithmic transformation the time (ltd).}
\value{\code{formula} with the forms: 'resp ~ cov | group' or '~ cov'.}
\references{Zeide B. 1993. Analysis of Growth Equations. For. Sci., 39: 594-616.                    }
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## an ltd formula:
lev <- c('plot','tree')
tdeq <- tdForm(lev,log.t = TRUE)
tdeq
## (not run) only primary covariate:
tdeq1 <- tdForm(lev,prim.cov = TRUE)
tdeq1


}
