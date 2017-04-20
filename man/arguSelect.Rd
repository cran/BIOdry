\name{arguSelect}
\alias{arguSelect}
\title{Argument selection}
\description{Arguments of specific functions are selected from arbitrary numbers
and varieties of arguments.}
\usage{arguSelect(rd = NULL, fun = c("mapply", "ringApply"), ...)}
\arguments{
  \item{rd}{\code{NULL} or \code{data.frame}. Multilevel
ecological data series. If \code{NULL} then this
argument is ignored.}
  \item{fun}{\code{character} or
\code{NULL}. Vector of function
names.}
  \item{\dots}{Further arguments not necessarily contained in the
processed function(s).}
}
\details{Closures with ellipsis terms use this
function to extract and pass arguments to
other functions. Arguments in \code{MoreArgs}
lists are also extracted and stored again as
\code{MoreArgs} lists.}
\value{\code{list} of selected arguments.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{

##Multilevel ecological data series of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    

## Selection of arguments in some functions:
ar1 <- arguSelect(fun = c('amod'),
                  only.dup = TRUE,
                  mp = c(0.5,1),
                  rf.t = 2003)
str(ar1)

ar2 <- arguSelect(fn = 'amod',
                  only.dup = TRUE,
                  mp = c(0.5,1),
                  rf.t = 2003)
str(ar2)
ar3 <- arguSelect(rd = Prings05,
                  fn = 'amod',
                  only.dup = TRUE,
                  mp = c(0.5,1),
                  rf.t = 2003)
str(ar3)

ar4 <- arguSelect(rd = Prings05,
                  fun = 'scacum',
                  sc.c = Pradii03,
                  MoreArgs = list(only.dup = TRUE,
                                  mp = c(0.5,1),
                                  rf.t = 2003))
str(ar4)

ar5 <- arguSelect(rd = Prings05,
                  fun = 'scacum',
                  ref = Pradii03,
                  rf.t = rep(2003:2011),
                  MoreArgs = list(only.dup = TRUE,
                                  mp = c(0.5,1)))
str(ar5)    

}
