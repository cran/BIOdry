\name{rtimes}
\alias{rtimes}
\title{Time-units synchronization}
\description{Unique observations in time-series replicates are excluded}
\usage{rtimes(x, only.dup = TRUE, 
    lv = NULL, validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{x}{multilevel ecological data series containing a column of
time units, or \code{numeric} vector with names representing
the time units. In a data frame the time units are found by the
package's column names: \code{year} (or \code{hydro_year_id}
from \code{\link{moveYr}}), \code{month} and \code{time}; a
date column is also used. Without any of them, row order is
used and a warning is given.}
  \item{only.dup}{\code{logical}. Extract only duplicated
times. If TRUE then unique times are replaced
with NA. If all computed times are unique then
this argument is ignored. Rows with NA time, for
example the first years of a core that no other
core of its tree covers (16 rows of
\code{Prings05} with \code{lv = 2} in
\code{\link{ringApply}}), are later dropped from
any model that uses \code{time} (see
\code{\link{frametoLme}}).}
  \item{lv}{optional split level passed to \code{slitFrame()}.
When supplied, synchronized times are computed
within each split block rather than on the whole
table.}
  \item{validate}{validation mode.}
}

\value{\code{data.frame} object with the initial vector and its time
units.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




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
