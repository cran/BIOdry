\name{scacum}
\alias{scacum}
\title{Scaled-cummulative sums}
\description{Cummulative sums of a vector are scaled on a constant value. }
\usage{scacum(x, y = NA, z = NA)}
\arguments{
  \item{x}{\code{numeric} vector with vector names being ordered on time}
  \item{y}{\code{NA} or \code{numeric} constant to scale the
vector.  If \code{NA} then the cummulative sums
are not scaled.}
  \item{z}{\code{NA} or \code{numeric} constant in range of
the vector names. If NA then maximun value in such a
range is used.}
}
\details{Cummulative values of the numeric vector are computed with \code{\link{cumsum}} }
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
