\name{arguSelect}
\alias{arguSelect}
\title{Argument selection}
\description{Getting arguments in function(s).                        }
\usage{arguSelect(rd = NULL, fun = c("mapply", "ringApply"), ...)}
\arguments{
  \item{rd}{\code{NULL} or \code{data.frame} object with
factor-level columns (see details).}
  \item{fun}{\code{character} vector with
name(s) of formulated functions.}
  \item{\dots}{Further arguments not necessarily required by formulated
function(s). Arguments in \code{MoreArgs} lists are also
processed.}
}
\details{This function is implemented by \code{\link{ringApply}} to model multilevel data in \code{rd}. But, the function may also be used for other purposes.  }
\value{\code{list} of arguments.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    

## getting arguments in some functions:
ar1 <- arguSelect(fun = c('amod'),
                  only.dup = TRUE,mp = c(0.5,1),z = 2003)
str(ar1)

ar2 <- arguSelect(fn = 'amod',
                  only.dup = TRUE,mp = c(0.5,1),z = 2003)
str(ar2)
ar3 <- arguSelect(rd = Prings05,fn = 'amod',
                  only.dup = TRUE,mp = c(0.5,1),z = 2003)
str(ar3)

ar4 <- arguSelect(rd = Prings05,
                  fun = 'scacum',y = Pradii03,
                  MoreArgs = list(only.dup = TRUE,
                                  mp = c(0.5,1),z = 2003))
str(ar4)

ar5 <- arguSelect(rd = Prings05,
                  fun = 'scacum',y = Pradii03,z = rep(2003:2011),
                  MoreArgs = list(only.dup = TRUE,
                                  mp = c(0.5,1)))
str(ar5)    

}
