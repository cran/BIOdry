\name{plot.wlai}
\alias{plot.wlai}
\title{Plot an wlai object}
\description{A Walter-Lieth climate diagram is produced.}
\usage{\method{plot}{wlai}(x, ...)}
\arguments{
  \item{x}{\code{vector} or \code{data.frame}. An object inheriting
from class \code{\link{wlai}}, representing the Aridity Index.}
  \item{\dots}{\code{logical}. Further arguments passed to
\code{\link{plot}} function.}
}
\details{Areas between temperature and
precipitation lines when precipitation
exceeds temperature (moist seasons) are
plotted in gray color, and areas where
temperature exceeds precipitation (dry
seasons) are plotted in black color. Monthly
cumulative precipitations over 100 mm are
scaled such that 1 degree C of average
temperature is equal to 5 mm of
precipitation.}
\value{A \code{\link{plot}} of the Walter-Lieth diagram.}
\references{Manrique E.,
A. Fernandez-Cancio. 2000. Extreme climatic events
in dendroclimatic reconstructions from
Spain. Clim. Chang., 44: 123-138.}
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##random simulation of climatic records
set.seed(1)
pr <- rnorm(12,1,1)
tm <- rnorm(12,0,1)
cld <- data.frame(pr,tm)
##labels of months from october to september
rownames(cld) <- month.abb[c(10:12,1:9)]
rownames(cld) <- c(10:12,1:9)
##computation of the aridity index and climate diagram
AI <- wlai(cld)
plot.wlai(AI)
}
