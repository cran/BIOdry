\name{scacum}
\alias{scacum}
\title{Cummulative-scaled sums}
\description{This function computes cummulative sums of time-series replicates.}
\usage{scacum(x, y = NA, z = NA)}
\arguments{
  \item{x}{\code{numeric} vector of time-series replicates with names
of the vector being time units.}
  \item{y}{constant, or vector if the processed time-series
replicates have several levels, to scale the
computed cumulative values.  If NA then the
computed cumulative sums are not scaled.}
  \item{z}{\code{NA}, or \code{numeric} constant. Reference
time(s) in range(s) of the vector names to scale the
cumulative values. If NA then maximum value in the
time-units range is used.}
}
\details{Cummulative sums of time-series replicates
(e.g. tree-radial increments) are scaled on reference constants
(e.g. individual tree diameters).}
\value{data frame with the original vector, and its scaled-cummulative sums.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
x <- c(0.79,0.32,0.53,0.43,0.18)
names(x) <- 1948:1952
y <- 4
z <- 1951
scacum(x,y,z)

##If y = NA then cummulative values are scaled arround
##max(cumsum(x)):
max(cumsum(x))
scacum(x,NA)
}
