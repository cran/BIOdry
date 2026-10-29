\name{print.frametoLme}
\alias{print.frametoLme}
\title{Print frametoLme objects}
\description{A compact printed representation of a \code{\link{frametoLme}} object is obtained.}
\usage{\method{print}{frametoLme}(x, ...)}
\arguments{
  \item{x}{Object inheriting from class \code{"frametoLme"}.}
  \item{\dots}{Additional arguments passed to \code{\link{summary.frametoLme}}.}
}
\details{This method prints the compact summary
instead of the raw list returned by
\code{\link{frametoLme}}, so users do not see the
full nested list with model, call, and residual
series every time the object name is evaluated.}
\value{The compact printed representation.}

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
## printing shows the compact summary, not the nested list
dfm2 |>
    frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
               random = pdDiag(~ log(csx))) |>
    print()
}
