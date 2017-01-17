\name{rtimes}
\alias{rtimes}
\title{Time-units synchronization}
\description{Unique observations in time-series replicates are excluded}
\usage{rtimes(x, only.dup = TRUE)}
\arguments{
  \item{x}{multilevel ecological data series containing a column of
time units, or \code{numeric} vector with names representing
the time units.}
  \item{only.dup}{\code{logical}. Extract only duplicated
times.  If TRUE then unique times are replaced
with NA. If all computed times are unique then
this argument is ignored.}
}

\value{\code{data.frame} object with the initial vector and its time
units.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## row names of a vector
fy <- function(y,span){(y - span):y}
x <- c(fy(2005,5),fy(2007,10)) 
## (not run) Simulating the vector
r <- abs(rnorm(length(x)))
names(r) <- x
## (not run) computing the synchronized times:
rtimes(r,only.dup = TRUE)        
## (not run) Extracting only duplicated times:
na.omit(rtimes(r,only.dup = TRUE))
}
