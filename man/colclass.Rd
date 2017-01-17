\name{colclass}
\alias{colclass}
\title{Column classes}
\description{Column names of multilevel ecological data sets are grouped into
three classes: \code{numeric}, time unit,
or \code{factor}.}
\usage{colclass(rd, as.list = TRUE)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{as.list}{\code{logical}. List the classes. If
\code{TRUE} then column names of the data are grouped
in a list object.}
}

\value{\code{character} vector of the column names, or \code{list} of
grouped column names.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel ecological data series of tree-ring widths:
data(Prings05,envir = environment())
## getting grouped column names    
classes <- colclass(Prings05)
str(classes)
}
