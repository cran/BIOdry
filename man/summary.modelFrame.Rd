\name{summary.modelFrame}
\alias{summary.modelFrame}
\title{summarize a modelFrame object}
\description{A summary of a \code{\link{modelFrame}} object is obtained.}
\usage{\method{summary}{modelFrame}(object, ...)}
\arguments{
  \item{object}{an object inheriting from class
\code{\link{modelFrame}}.}
  \item{\dots}{additional optional arguments passed to
\code{\link{summary.lme}} method.}
}



\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## An object from class \code{\link{summary.lme}}.
data(Pchron,Pradii03,envir = environment())
## Tree-ring width fluctuations:
trwf <- modelFrame(Pchron,
                   sc.c = Pradii03,
                   rf.t = 2003,
                   log.t = TRUE)
summary(trwf)
}
