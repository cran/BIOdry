\name{plot.modelFrame}
\alias{plot.modelFrame}
\title{Plot modelFrame objects}
\description{Diagnostic Trellis plot for fluctuations in
\code{\link{modelFrame}} objects are obtained.}
\usage{\method{plot}{modelFrame}(x, ...)}
\arguments{
  \item{x}{An object inheriting from class \code{\link{modelFrame}}.}
  \item{\dots}{further arguments passed to the Trellis plot function.}
}



\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##TRW chronology (mm) and reference inside-bark radii (mm)
##measured at 2003:
data(Pchron,envir = environment())
data(Pradii03,envir = environment())
## Tree-ring width fluctuations:
trwf <- modelFrame(Pchron,
                   sc.c = Pradii03,
                   rf.t = 2003,
                   log.t = TRUE)
plot(trwf, grid = FALSE)

}
