\name{levexp}
\alias{levexp}
\title{Vector releveling}
\description{Expansion or reduction of a numeric vector by matching its level names
with the ecological factors of a multilevel ecological data
series.}
\usage{levexp(x, levels)}
\arguments{
  \item{x}{\code{numeric} vector with names of the vector representing
the \code{levels} to be matched.}
  \item{levels}{\code{data.frame}. Multilevel ecological data series,
or \code{character} vector of \code{levels}.}
}

\value{numeric vector with expanded/reduced \code{levels}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
##Multilevel ecological data series of tree-ring widths:
data(Prings05,envir = environment())
## tree radii measured at 2003:
data(Pradii03,envir = environment())    

## Releveling the tree radii
refs <- levexp(Pradii03,Prings05)
refs
}
