\name{PTclim05}
\docType{data}
\alias{PTclim05}
\title{Meteorological records of precipitation and temperature  
}
\description{Monthly precipitation sums and
  average temperatures of two climatic locations. 
}
\usage{data(PTclim05)}
\format{
      A data frame with the following 5 variables.
    \describe{
      \item{\code{pr}}{A numeric vector with the monthly precipitation
	sums measured in mm month-1}
      \item{\code{tm}}{A numeric vector with the monthly mean temperatures
	measured in degree C month-1}      
      \item{\code{month}}{A numeric vector with the recorded month}
      \item{\code{year}}{A numeric vector with the recorded year}
      \item{\code{plot}}{A factor indicating the code of a correspondant dendrochronological plot}
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
