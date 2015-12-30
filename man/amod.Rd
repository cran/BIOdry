\name{amod}
\alias{amod}
\title{Allometric modeling}
\description{Parameters in simple allometric model are evaluated on tree radial increments to compute diameters, basal areas, tree biomasses, etc.}
\usage{amod(cs, mp = c(0.5, 1), un = NULL)}
\arguments{
  \item{cs}{\code{Numeric} vector of scaled-cummulative sums such as
that produced by \code{\link{scacum}}.}
  \item{mp}{\code{Numeric}. vector with allometric
parameters. Default \code{c(0.5,1)} maintains the
original radii (see details for other variables)}
  \item{un}{NULL, or bidimensional \code{character} vector to
transform SI units of the processed variable. The SI
units can be expressed in micrometers 'mmm',
milimeters 'mm', centimeters 'cm', decimeters 'dm', or
meters 'm'. If NULL then original units are
maintained.}
}
\details{The simple allometric model has the form: a * cs ^ b, with a,b being constants in \code{mp}, and cs being scaled-cummulative sums.  Different dendrometric variables can be computed; for example, \code{c(1,1)} produces diameters, and \code{c(0.25 * pi,2)} computes basal areas. Argument \code{mp} can have more than two parameters: \code{c(a1,b1,a2,b2, ..., an,bn)}, with \code{n} being the number of times that allometric model will be recursively implemented. Such recursive evaluation is useful to derive variables which depend on other allometric covariables: i.e allometric model would be implemented twice to recursively compute diameters and tree biomasses. A column of increments of cs (x) is also computed by implementing \code{\link{setdiff}}. }
\value{\code{data.frame} object.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## radial increments
set.seed(1)
w <- abs(rnorm(12,1,1))
names(w) <- 1951:1962
## scaled and cummulative radial increments
sr <- scacum(w)
## diameters
d <- amod(sr[,2],c(1,1))
## basal areas (m2):
ba <- amod(sr[,2],c(0.25 * pi,2),c('mm','m'))
print(ba)
}
