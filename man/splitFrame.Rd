\name{splitFrame}
\alias{splitFrame}
\title{Multilevel splitting }
\description{This function splits a multilevel ecological data series (MEDS) based on
the levels of an ecological factor.}
\usage{splitFrame(rd, lv = 1)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{lv}{\code{Numeric} or \code{character}. Position number of
the ecological factor(s) in the MEDS, or correspondent
column name(s), to split the data. If the splitting
column is not a factor, the character name of the column
should be used.}
}
\details{This function is implemented by
\code{\link{ringApply}} and
\code{\link{modelFrame}} functions to
develop multilevel modeling of MEDS.}
\value{\code{list} of \code{data.frame} objects.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Dendrochronological MEDS:
data(Prings05,envir = environment())

## split the MEDS by levels contained in its second ecological
## factor:
spl <- splitFrame(Prings05,2)
str(spl)
## split the data by levels defined in the 'year' factor:
spl <- splitFrame(Prings05,'year')
str(spl)
}
