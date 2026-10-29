\name{print.summary.frametoFriedman}
\alias{print.summary.frametoFriedman}
\title{Print compact frametoFriedman summaries}
\description{A compact printed summary of a \code{\link{frametoFriedman}} object is obtained.}
\usage{\method{print}{summary.frametoFriedman}(x, 
    digits = max(3L, 
        getOption("digits") - 
            2L), ...)}
\arguments{
  \item{x}{Object inheriting from class \code{"summary.frametoFriedman"}.}
  \item{digits}{Integer number of \code{digits}.}
  \item{\dots}{Unused extra arguments.}
}

\value{The printed compact summary.}

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
dfm2 |> frametoFriedman() |> summary() |> print()
}
