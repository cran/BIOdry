\name{colclass}
\alias{colclass}
\title{Column classes}
\description{Column names in multilevel data frames are grouped into classes according to three level types: \code{numeric}, temporal, or \code{factor}. }
\usage{colclass(rd, as.list = TRUE)}
\arguments{
  \item{rd}{\code{data.frame} object with factor-level columns.}
  \item{as.list}{\code{logical}. List the classes. If
\code{TRUE} then column names in data are
grouped in the form of a list.}
}

\value{\code{character} vector of the column names, or \code{list} of grouped
column names.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## getting grouped column names    
classes <- colclass(Prings05)
str(classes)
}
