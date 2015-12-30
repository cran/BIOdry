\name{rtimes}
\alias{rtimes}
\title{Relative times}
\description{Relative times from 1 to the number of observed years are computed. Duplicated times can be sinchronized by replacing unique values with NA.   }
\usage{rtimes(x, only.dup = TRUE)}
\arguments{
  \item{x}{\code{numeric} vector with names representing the
formation years, or multilevel data frame containing a column
of years.}
  \item{only.dup}{\code{logical}. Extract only duplicated
times.  If TRUE then unique times are replaced
with NA. If all computed times are unique then
this argument is ignored.}
}

\value{\code{data.frame} object with the initial vector and its formation times.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## row names of a vector
fy <- function(y,span){(y - span):y}
x <- c(fy(2005,5),fy(2007,10)) 
## (not run) Simulating the vector
r <- abs(rnorm(length(x)))
names(r) <- x
## (not run) computing relative times:
rtimes(r,only.dup = TRUE)        
## (not run) Extracting only duplicated times:
na.omit(rtimes(r,only.dup = TRUE))
}
