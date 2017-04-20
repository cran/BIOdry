\name{levexp}
\alias{levexp}
\title{Vector releveling}
\description{Expansion or reduction of a numeric vector by matching its level names
with the ecological factors of a multilevel ecological data
series.}
\usage{levexp(x, levels)}
\arguments{
  \item{x}{\code{numeric} vector with names of the vector representing
the levels to be matched.}
  \item{levels}{\code{data.frame}. Multilevel ecological data series,
or \code{character} vector of levels.}
}

\value{numeric vector with expanded/reduced levels.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel ecological data series of tree-ring widths:
data(Prings05,envir = environment())
## tree radii measured at 2003:
data(Pradii03,envir = environment())    

## Releveling the tree radii
refs <- levexp(Pradii03,Prings05)
refs
}
