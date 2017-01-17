\name{amod}
\alias{amod}
\title{Allometric modeling}
\description{Parameters of simple allometric model are evaluated to derive
allometric components of organisms from longitudinal variables.}
\usage{amod(cs, mp = c(0.5, 1), un = NULL)}
\arguments{
  \item{cs}{\code{Numeric}. Sequence of mesures of an organic
component.}
  \item{mp}{\code{Numeric}. vector with allometric
parameters. Default \code{c(0.5,1)} maintains the
original radii (see details for other variables)}
  \item{un}{NULL, or bidimensional \code{character} vector to
transform SI units of the processed variables. The SI
units can be expressed in micrometers 'mmm',
milimeters 'mm', centimeters 'cm', decimeters 'dm', or
meters 'm'. If NULL then original units are
maintained.}
}
\details{The simple allometric model has the form:
a * cs ^ b, with a,b being constants in \code{mp},
and cs being a sequence of mesures of an organic
component (e.g. the tree-radial increments).
Different allometric variables can be computed;
for the case of tree-radial increments, \code{mp =
c(1,1)} produces diameters, and \code{mp = c(0.25
* pi,2)} computes basal areas. The argument
\code{mp} can have more than two parameters:
\code{c(a1,b1,a2,b2, ..., an,bn)}, with \code{n}
being the number of times that allometric model
will be recursively implemented. Such recursive
evaluation is useful to derive variables which
depend on other allometric covariables: i.e
allometric model would be implemented twice to
recursively compute diameters and tree biomasses
from tree-ring widths. A column of increments of
cs (x) is also computed for further in-package
modeling of relative organic growth from
cumulative organic growth (see
\code{\link{tdForm}} function).}
\value{\code{data.frame} of computed allometric-organic components and
their correspondent relative increments.}

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
