\name{scacum}
\alias{scacum}
\title{Cummulative-scaled sums}
\description{This function computes cummulative and scaled sums of time-series
replicates.}
\usage{scacum(x, sc.c = NA, 
    rf.t = NA)}
\arguments{
  \item{x}{\code{numeric} vector of time-series replicates with names
of the vector being time units.}
  \item{sc.c}{\code{numeric} constant. Scaling constant.  If
\code{NA} then the computed cumulative sums are not
scaled.}
  \item{rf.t}{\code{NA}, or \code{numeric} constant. Reference
time of the scaling constant. If \code{NA} then
maximum time in vector-name range is used.}
}
\details{Cummulative sums of time-series
replicates (e.g. tree-ring widths) are scaled
around reference values (e.g. tree radii).}
\value{data frame with the original vector, and its scaled-cummulative sums.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
x <- c(0.79,0.32,0.53,0.43,0.18)
names(x) <- 1948:1952
scacum(x,sc.c = 4,rf.t = 1951)

##If sc.c = NA then cummulative values are scaled arround
##max(cumsum(x)):
max(cumsum(x))
scacum(x,NA,1951)
}
