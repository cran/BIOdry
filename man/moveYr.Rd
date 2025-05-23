\name{moveYr}
\alias{moveYr}
\title{Seasonal years}
\description{Monthly records in time-series replicates (usually of climate) are
labeled for the years can begin in a month other than January.}
\usage{moveYr(cd, ini.mnt = "Oct")}
\arguments{
  \item{cd}{\code{data.frame}. Multilevel ecological data series or
\code{numeric} vector of repeated years with vector names
belonging to \code{month.abb}.}
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

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Climatic records of monthly precipitation sums and monthly
## average temperatures
data(PTclim05,envir = environment())

## Making the year 1955 in plot 'P16106' to begin on 'April'
cl1 <- slitFrame(PTclim05,c('year','plot'))[[1]]
cl2 <- moveYr(cl1,ini.mnt = 'Mar')
head(cl2)

## a simple vector of years
yr <- rep(2005,12)
names(yr) <- month.abb[1:12]
moveYr(yr)

}
