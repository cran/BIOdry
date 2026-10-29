\name{scacum}
\alias{scacum}
\title{scacum}
\description{This function computes cumulative and optionally scaled sums of ordered
time-series replicates.}
\usage{scacum(x, sc.c = NA, 
    rf.t = NA, lv = NULL, 
    validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{x}{Numeric vector of time-series replicates, or a data frame.}
  \item{sc.c}{Numeric scaling constant. If \code{NA}, cumulative sums
are not scaled.}
  \item{rf.t}{Reference time for scaling. If \code{NA}, the last time
value of each ordered block is used. If given, the data
must have a \code{year}, \code{hydro_year_id} or
\code{time} column; otherwise \code{scacum()} stops.}
  \item{lv}{Optional split level for multilevel data frames. Can be
\code{NULL}, character, or numeric, and is resolved through
\code{slitFrame()}.}
  \item{validate}{Validation mode.}
}
\details{Cumulative sums of time-series replicates
(e.g. tree-ring widths) can be computed on a single
ordered block, or within multilevel data frames after
splitting by a specified level. For data frames,
\code{scacum()} orders each block on available time
variables before computing cumulative sums and returns
the original data with an added \code{csx} column.
The value column is \code{x}, the package's column
name; a response declared with
\code{\link{defineDataRoles}} takes precedence.
Without either, \code{scacum()} stops.
The time variables are found by the package's
column names: \code{year} (or \code{hydro_year_id}
from \code{\link{moveYr}}), \code{month} and
\code{time}; \code{rf.t} refers to the first of
\code{hydro_year_id}, \code{year} and \code{time}
that is present.}


\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
x <- c(0.79,0.32,0.53,0.43,0.18)
names(x) <- 1948:1952
scacum(x, sc.c = 4, rf.t = 1951)

max(cumsum(x))
scacum(x, NA, 1951)
}
