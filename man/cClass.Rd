\name{cClass}
\alias{cClass}
\title{Column-class extraction.}
\description{Column names of multilevel data sets are extracted according to
three classes: \code{numeric} values, \code{integer} sequences,
and \code{factor} levels.}
\usage{cClass(rd, cl = "all")}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel  data series.}
  \item{cl}{\code{character} or \code{NULL}. Character vector
of classes to be considered. These can be
'numeric', 'integer', or 'factor'. If \code{'all'}
then all column names of \code{rd} are extracted.}
}

\value{\code{character} names.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())
## Names of variables in Prings05 data containing numeric classes:
cClass(Prings05, 'numeric') # 'x'
## Names of variables containing time units: 
cClass(Prings05, 'integer') # 'year'
## Names of variables containing factors: 
cClass(Prings05, 'factor') # 'sample', 'tree', 'plot'

}
