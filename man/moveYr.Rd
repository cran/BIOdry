\name{moveYr}
\alias{moveYr}
\title{Seasonal years}
\description{Monthly records in time-series replicates (usually of climate) are
labeled for the years can begin in a month other than January.}
\usage{moveYr(cd, ini.mnt = "Oct")}
\arguments{
  \item{cd}{\code{data.frame}. Multilevel ecological data series or
\code{numeric} vector of repeated years with vector names
being the months.}
  \item{ini.mnt}{\code{character}, or \code{numeric} from 1 to
12. Initial month of the seasonal year. If
\code{character} then the months are built-in
constants in R-package \code{base}. Default
\code{'Oct'} makes the years begin in October,
for example.}
}
\details{\code{character} months as defined in
\code{\link{month.abb}} or \code{\link{month.name}}.}
\value{\code{data.frame} object with the months being \code{numeric}
values and the years beginning at \code{ini.mnt} argument.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## Climatic records of monthly precipitation sums and monthly
## average temperatures
data(PTclim05,envir = environment())

## Making the year 1955 in plot 'P16106' to begin on 'April'
cl1 <- splitFrame(PTclim05,'year')[['P16106.1955']]
cl2 <- moveYr(cl1,ini.mnt = 4)
head(cl2)

## a simple vector of years
yr <- rep(2005,12)
names(yr) <- month.abb[1:12]
moveYr(yr)

}
