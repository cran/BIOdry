\name{summary.frametoLme}
\alias{summary.frametoLme}
\title{Summarize frametoLme objects}
\description{A compact summary of a \code{\link{frametoLme}} \code{object} is obtained.}
\usage{\method{summary}{frametoLme}(object, 
    ...)}
\arguments{
  \item{object}{Object inheriting from class \code{"frametoLme"}.}
  \item{\dots}{Additional arguments passed to
\code{\link[nlme]{summary.lme}}.}
}
\details{This method extracts the main model
components that users typically inspect after
fitting a mixed-effects model: the original
package call, fitting method, sample size,
likelihood-based fit statistics, residual
standard deviation, grouping sizes, fixed-effect
table, and random-effect variance structure. It
avoids the very long Data line (a deparsed
\code{structure()} call) shown by \code{\link[nlme]{summary.lme}}
when the fitted \code{lme} \code{object} stores an
inlined \code{groupedData} call.}
\value{A compact summary \code{object} of class \code{"summary.frametoLme"}.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
## relative time within trees, then cumulative growth scaled to the radii
data(Prings05, envir = environment())
data(Pradii03, envir = environment())
dfm2 <- Prings05 |>
    ringApply(lv = 2, fn = 'rtimes') |>
    ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
              fn = 'scacum')
s <- dfm2 |>
    frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
               random = pdDiag(~ log(csx))) |>
    summary()
## fit statistics and fixed effects
s$AIC
s$fixed
}
