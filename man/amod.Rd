\name{amod}
\alias{amod}
\title{Allometric scaling.}
\description{Allometric models and parameters are used to scale organic growth.}
\usage{amod(x, mp = c(1, 1), fun = y ~ a * (x^b))}
\arguments{
  \item{x}{\code{numeric} vector.}
  \item{mp}{\code{numeric}. Allometric
parameters. Default
\code{c(1,1)} (see
details).}
  \item{fun}{\code{formula}.
Allometric
model. To properly
specify other
formulas, the
variables (e.g. x
and y) should
belong to
\code{letters[20:26]}.}
}
\details{. Allometric models are useful to
scale size-components of organisms such as
tree diameters (mp = \code{c(2,1)}) and
basal areas (mp = \code{c(0.25 *
pi,2)}). Several parameter groups
(\code{c(a1,b1,a2,b2, ..., an,bn)}) can be
recursively processed.  This enables
computation of complex organic
variables. For example, above-ground tree
biomass could be computed from two
parameter groups for tree-biomass, and
over-bark diameter scaling.}
\value{\code{data.frame} of the scaled variable (x) and relative
increments (csx). These are computed with \code{\link{setdiff}}
function.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## Simulating TRW records:
set.seed(1)
trw <- ts(abs(rnorm(12,1,1)),start = 1950)
## Cumulative TRW:
cri <- cumsum(trw)
## tree diameters
td <- amod(cri,mp = c(2,1))
## plot of the tree diameters and the
## relative increments:
plot(ts(td))
}
