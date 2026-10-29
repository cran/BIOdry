\name{summary.frametoFriedman}
\alias{summary.frametoFriedman}
\title{Summarize frametoFriedman objects}
\description{A compact summary of a \code{\link{frametoFriedman}} \code{object} is obtained.}
\usage{\method{summary}{frametoFriedman}(object, 
    ...)}
\arguments{
  \item{object}{Object inheriting from class \code{"frametoFriedman"}.}
  \item{\dots}{Unused extra arguments.}
}
\details{This method extracts the main
components that users typically inspect after
Friedman detrending: the original call, the chosen
response and time variables, the series hierarchy,
the smoothing settings, the number of observations
and series, and a per-series status table.}
\value{A compact summary \code{object} of class \code{"summary.frametoFriedman"}.}

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
s <- dfm2 |> frametoFriedman() |> summary()
## settings and the status of each series
s$span
s$series
}
