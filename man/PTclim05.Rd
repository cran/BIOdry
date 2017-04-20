\name{PTclim05}
\docType{data}
\alias{PTclim05}
\title{Multilevel data set of monthly cumulative precipitations and
  monthly average temperatures  
}
\description{Monthly precipitation sums and
  average temperatures of two climatic locations. 
}
\usage{data(PTclim05)}
\format{
      A data frame with the following 5 variables.
    \describe{
      \item{\code{pr}}{A numeric vector with the monthly cumulative precipitations
	 (mm month-1)}
      \item{\code{tm}}{A numeric vector with the monthly average temperatures
	(degree C month-1)}      
      \item{\code{month}}{A numeric vector of months}
      \item{\code{year}}{A numeric vector of years}
      \item{\code{plot}}{A factor vector of plot codes}
      }
}

\details{Recorded months begin at January and
end at December, and observed years span from 1951 to
2005. Factor in data set defines one sample level (plot)
}
\references{
  Bogino, S., and Bravo, F. (2008). Growth response of Pinus pinaster Ait. to climatic variables in central Spanish forests. Ann. For. Sci. 65, 1-13.
}
\examples{
str(PTclim05)
}
\keyword{data sets}
