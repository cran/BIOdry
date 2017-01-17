\name{Prings05}
\docType{data}
\alias{Prings05}
\title{MEDS of Pinus pinaster
}

\description{Radial increments of Pinus pinaster from two sample plots
 in northern (Soria) and east-central Spain (Cuenca).  }

\usage{data(Prings05)}
\format{
    A data frame with the following 5 variables.
    \describe{
      \item{\code{x}}{numeric vector with the radial increments in mm
	year-1}
      \item{\code{year}}{numeric vector with the recorded year}
      \item{\code{sample}}{ecological factor indicating the sample replicate}
      \item{\code{tree}}{ecological factor indicating the number of measured tree}
      \item{\code{plot}}{ecological factor indicating the code of measured plot}
      }
    }
    
\details{This dataset contains eight series of tree-ring widths of
maritime pine (Pinus pinaster Ait.), with recorded years spanning from 1810
to 2005. The cores were sampled from dominant trees of two sites, with
sample plots being located on both: northern (plot code: P44005) and
east-central Spain (plot code: P16106). Two trees were selected by plot,
and two core samples were extracted by tree. Consequently, the sample
design defined three levels: sample in tree on plot (plot level), sample
in tree (tree level), and sample level.  }

\references{Bogino, S., and Bravo, F. (2008). Growth response of Pinus pinaster Ait. to climatic variables in central Spanish forests. Ann. For. Sci. 65, 1-13.
}
\examples{
str(Prings05)
}
\keyword{datasets}
