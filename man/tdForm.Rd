\name{tdForm}
\alias{tdForm}
\title{ltd formulas}
\description{This function formulates linear time-decline formulas (ltd) from
categorical variables in multilevel ecological data series.}
\usage{tdForm(rd, prim.cov = FALSE, 
    on.time = TRUE, log.t = FALSE, 
    lev.rm = NULL, validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{rd}{\code{data.frame} or \code{character} vector. Multilevel
ecological data series or vector of ecological factors.}
  \item{prim.cov}{\code{logical}. Print a primary covariate
form: \code{'~ cov'}. If FALSE then a complete
formula: \code{'resp ~ cov | group'} is printed.}
  \item{on.time}{\code{logical}. If TRUE (default) the relative
time column \code{'time'} from \code{\link{rtimes}}
is used when present, otherwise \code{'year'}.
If FALSE \code{'year'} is used when
present, otherwise \code{'time'}. A time variable
declared with \code{defineDataRoles(time = ...)}
takes precedence over both.}
  \item{log.t}{\code{logical}. If TRUE then \code{f(time) =
ln(time)}. Default FALSE produces a log-linear
time-decline formula. Both settings are Zeide forms,
f(time) = time (FALSE) and f(time) = ln(time) (TRUE),
with the same number of parameters; fit both by
maximum likelihood (\code{method = "ML"} in
\code{\link{frametoLme}}) and choose by AIC or BIC.}
  \item{lev.rm}{NULL or \code{character} name of the ecological
factor(s) in the MEDS to be removed from the
formula.}
  \item{validate}{\code{character}. Validation mode: \code{'none'} silences checks, \code{'warn'} emits warnings, and \code{'stop'} stops on invalid inputs.}
}
\details{the ltd formulas belong to following
general equation: log (x) = log (csx) + f(time);
where the relative organic growth (x) is
explained by the cumulative organic growth (csx)
plus a function of time f(time); with f(time)
being either the time or a logarithmic
transformation the time. The ltd can be
implemented by \code{\link{modelFrame}} function
to subtract trends in organic MEDS.

The response \code{x}, the cumulative growth
\code{csx} and the time covariate (\code{time}
from \code{\link{rtimes}}, or \code{year}) are the
package's fixed column names. The grouping levels
are the factor and character columns other than
the month column, outer level first (the reverse
of the column order; see \code{\link{cClass}}). A
time variable or grouping
levels declared with \code{\link{defineDataRoles}}
take precedence; the metadata that
\code{\link{ringApply}} records automatically does
not.}
\value{\code{formula} with the forms: 'resp ~ cov | group' or '~ cov'.}
\references{Zeide B. 1993. Analysis of Growth
Equations. For. Sci., 39: 594-616.}
\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




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
