\name{PTclim05}
\docType{data}
\alias{PTclim05}
\title{MEDS of climate  
}
\description{Monthly precipitation sums and
  average temperatures of two climatic locations in northern (Soria)
  and east-central Spain (Cuenca).  
}
\usage{data(PTclim05)}
\format{
      data frame with the following 5 variables.
    \describe{
      \item{\code{pr}}{numeric vector of monthly precipitation
	sums (mm month-1)}
      \item{\code{tm}}{numeric vector of monthly mean temperatures
	(degree C month-1)}      
      \item{\code{month}}{numeric vector of recorded months}
      \item{\code{year}}{numeric vector of recorded years}
      \item{\code{plot}}{ecological factor indicating correspondent dendrochronological plot}
      }
}

\details{Recorded months begin at January and
end at December, and observed years span from 1951 to
2005. Factor in dataset defines one sample level (plot)
}
\references{
  Bogino, S., and Bravo, F. (2008). Growth response of Pinus pinaster Ait. to climatic variables in central Spanish forests. Ann. For. Sci. 65, 1-13.
}
\examples{
str(PTclim05)
}
\keyword{datasets}
