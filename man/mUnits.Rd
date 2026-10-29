\name{mUnits}
\alias{mUnits}
\title{Metric system}
\description{This function control metric units.}
\usage{mUnits(x, from = "mm", 
    to = "mm")}
\arguments{
  \item{x}{\code{numeric} vector.}
  \item{from}{\code{character}. Initial metric unit.}
  \item{to}{\code{character}. Final metric unit.}
}
\details{Characters in \code{from} and \code{to}
arguments have the form 'p_', where 'p' is the
metric prefix and '_' is a base unit.  Sixteen
metric prefixes are supported: atto 'a', femto
'f', pico 'p', nano 'n', micro 'mm', milli 'm',
centi 'c', deci 'd', deca 'da', hecto 'h', kilo
'k', mega 'M', giga 'G', tera 'T', peta 'P', and
exa 'E'.}
\value{\code{numeric} vector.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Simulation of TRW data
set.seed(1)
w <- abs(rnorm(12,1,1))
trw <- ts(w,start = 1970)
## transforming metric units of trw vector from millimeters to meters
sr <- mUnits(trw, from = 'mm', to = 'm')
attributes(sr)
}
