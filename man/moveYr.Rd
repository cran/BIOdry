\name{moveYr}
\alias{moveYr}
\title{Seasonal years}
\description{Level of years is processed for each of the years can begin in a month different than 'January'.}
\usage{moveYr(cd, ini.mnt = "Oct")}
\arguments{
  \item{cd}{\code{data.frame} object with factor-level columns of
months and years, or \code{numeric} vector of repeated years
with vector names being the months.}
  \item{ini.mnt}{\code{character} of initial month of the year
(see details), or \code{numeric} value from 1 to
12. Default 'Oct' produces years to begin in
October.}
}
\details{\code{character} months as defined in \code{\link{month.abb}} or \code{\link{month.name}}.  }
\value{\code{data.frame} object with the months being \code{numeric}
values and the years beguinning in \code{ini.mnt}}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## Climatic records of monthly precipitations and temperatures
data(PTclim05,envir = environment())

## Making year 1955 in plot 'P16106'to begin on 'April'
cl1 <- splitFrame(PTclim05,'year')[['P16106.1955']]
cl2 <- moveYr(cl1,ini.mnt = 4)
head(cl2)

## a simple vector of years
yr <- rep(2005,12)
names(yr) <- month.abb[1:12]
moveYr(yr)

}
