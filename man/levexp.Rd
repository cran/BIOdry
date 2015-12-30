\name{levexp}
\alias{levexp}
\title{Vector releveling}
\description{Expansion or reduction of a numeric vector by matching its levels with the factor-level columns in a data frame.}
\usage{levexp(x, levels)}
\arguments{
  \item{x}{\code{numeric} vector with names of the vector representing
the levels to be matched.}
  \item{levels}{\code{data.frame} with factor-level columns, or
\code{character} vector of levels.}
}

\value{numeric vector with expanded/reduced levels.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    

## Getting the factor-level names at sample level
ntl <- names(splitFrame(Prings05,'sample'))
## Releveling the radii
refs <- levexp(Pradii03,ntl)

}
