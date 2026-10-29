\name{frametoFriedman}
\alias{frametoFriedman}
\title{Friedman supersmoother detrending}
\description{Friedman smoothers are fitted to multilevel ecological series.}
\usage{frametoFriedman(rd, form = "FriedmanForm", 
    response = NULL, 
    time = NULL, groups = NULL, 
    res.data = TRUE, 
    difference = NULL, 
    span = NULL, bass = NULL, 
    periodic = NULL, 
    min.obs = NULL, validate = c("none", 
        "warn", "stop"), 
    ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{form}{\code{character}, \code{list}, or
\code{NULL}. Terminal smoother
specification.}
  \item{response}{\code{NULL} or \code{character}. Response series.}
  \item{time}{\code{NULL} or \code{character}. Time variable.}
  \item{groups}{\code{NULL} or \code{character}. Grouping variables.}
  \item{res.data}{\code{logical}. If \code{TRUE}, attach detrended
series in component \code{fluc}.}
  \item{difference}{\code{NULL} or \code{logical}. Use subtraction
instead of ratio detrending.}
  \item{span}{\code{NULL}, \code{character}, or \code{numeric}. Span.}
  \item{bass}{\code{NULL} or \code{numeric}. Bass argument.}
  \item{periodic}{\code{NULL} or \code{logical}. Periodic option.}
  \item{min.obs}{\code{NULL} or \code{integer}. Minimum finite
observations per series.}
  \item{validate}{\code{character}. Validation mode.}
  \item{\dots}{Unused extra arguments kept for compatibility.}
}
\details{This terminal fitter applies
\code{\link[stats]{supsmu}} series by series on a
multilevel long-format data frame. It resolves one
\code{response} variable, one primary \code{time} variable, and a
grouping hierarchy defining the independent series.

This patched version is constructor-aware. When a
\code{BIOdry_meta} attribute is available, the
resolved \code{time} variable is excluded from the Friedman
grouping hierarchy unless \code{time} was explicitly
declared as part of \code{groups}. This prevents accidental
smoothing of one-point series such as
\code{zone / hydro_year_id} when the intended fit
is one series per \code{zone} over
\code{hydro_year_id}.

The returned object is still a Friedman detrending
object, not a mixed model. If \code{res.data =
TRUE}, component \code{$fluc} contains detrended
series and fitted values for plotting and later
second-stage analyses.}
\value{\code{list} of class \code{"frametoFriedman"} with at least
components \code{model} and \code{call}, and optionally
\code{fluc}.}
\references{Friedman, J. H. 1984. A variable span
smoother. Laboratory for Computational Statistics,
Stanford University Technical Report 5.}
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
## Friedman supersmoother on each core; ratio index x / smooth
ff <- frametoFriedman(dfm2)
ff
head(ff$fluc)
## difference index x - smooth
frametoFriedman(dfm2, difference = TRUE)$fluc |> head()
}
