\name{FriedmanForm}
\alias{FriedmanForm}
\title{Friedman terminal specification}
\description{This function formulates Friedman smoother specifications for
multilevel ecological data series.}
\usage{FriedmanForm(rd, response = NULL, 
    time = NULL, groups = NULL, 
    difference = FALSE, 
    span = "cv", bass = 0, 
    periodic = FALSE, 
    min.obs = 4L, validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{response}{\code{NULL} or \code{character}. Name of the
\code{response} series to smooth.}
  \item{time}{\code{NULL} or \code{character}. Name of the \code{time}
variable. A vector is allowed; its first element
present in \code{rd} is used.}
  \item{groups}{\code{NULL} or \code{character}. Columns that
define individual series. Nested strings such as
\code{"plot/tree/sample"} are accepted.}
  \item{difference}{\code{logical}. Use subtraction instead of
ratio detrending.}
  \item{span}{\code{character} or \code{numeric}. Span argument
passed to \code{stats::supsmu()}.}
  \item{bass}{\code{numeric}. Bass argument passed to
\code{stats::supsmu()}.}
  \item{periodic}{\code{logical}. Periodic smoother option.}
  \item{min.obs}{\code{integer}. Minimum finite observations per
series.}
  \item{validate}{\code{character}. Validation mode.}
}
\details{Unlike \code{\link{tdForm}} and
\code{\link{lmeForm}}, this helper does not
return a regression formula. It returns a compact
terminal specification used by
\code{\link{frametoFriedman}} and
\code{\link{modelFrame}} to identify the \code{response}
series, the \code{time} variable, the grouping columns,
and the main smoothing settings.

The \code{time} variable is the first whole-number
numeric column and the grouping columns are the
factor and character columns other than the
month column (see \code{\link{cClass}}). Roles
declared with \code{\link{defineDataRoles}} take
precedence; the metadata that
\code{\link{ringApply}} records automatically does
not. The \code{time} variable is excluded from the
grouping columns unless the declared metadata
says that \code{time} also belongs to \code{groups}, so each
series is the \code{response} over \code{time} within \code{groups},
not within group-time combinations with fewer
than \code{min.obs} observations.}
\value{\code{list} inheriting from class \code{"FriedmanForm"}.}
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
## the specification resolved from the data: response, time, series
## and smoothing settings
FriedmanForm(dfm2) |> str()
## a difference index (x - smooth) instead of a ratio index (x / smooth)
FriedmanForm(dfm2, difference = TRUE)$difference
}
