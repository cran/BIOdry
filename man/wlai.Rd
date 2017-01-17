\name{wlai}
\alias{wlai}
\title{Walter-Lieth aridity index}
\description{Computing the annual aridity index from Walter-Lieth climate diagrams }
\usage{wlai(cd, sqt = TRUE, fig = FALSE, ...)}
\arguments{
  \item{cd}{\code{data.frame}. Multilevel climatic data series of
monthly precipitation sums (mm), and monthly average
temperatures (degree C), with row names being monthly
characters in \code{\link{month.abb}} or
\code{\link{month.name}}.}
  \item{sqt}{\code{logical}. Print the square root of the
aridity index.  If TRUE then computed aridity index
is normalized with a square root transformation.}
  \item{fig}{\code{logical}. Plot the Walter-Lieth diagram.}
  \item{\dots}{Further arguments to be passed to \code{\link{plot}}}
}
\details{Areas between temperature and
precipitation lines when precipitation exceeds
temperature are calculated as indicators of moist
seasons, and areas where temperature exceeds
precipitation are calculated as indicator of dry
season. The aridity index is defined as the
quotient between the areas of dry and wet
seasons. Those precipitations over 100 mm are
scaled such that 1 degree C is equal to 5 mm.}
\value{\code{numeric} aridity index and plot of the Walter-Lieth diagram.}
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
AI <- wlai(cld,fig = TRUE)
AI
}
