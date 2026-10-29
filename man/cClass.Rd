\name{cClass}
\alias{cClass}
\title{Column-class extraction}
\description{Column names of multilevel data sets are extracted according to
three classes, which BIOdry uses as column roles: \code{numeric}
(numeric columns that are not whole numbers; responses),
\code{integer} (whole-number numeric columns, whatever their
storage mode; time candidates), and \code{factor} (factor and
character columns; grouping candidates, except the month column).}
\usage{cClass(rd, cl = "all", 
    validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel  data series.}
  \item{cl}{\code{character} or \code{NULL}. Character vector
of classes to be considered. These can be
'numeric', 'integer', or 'factor'. If \code{'all'}
then all column names of \code{rd} are extracted.}
  \item{validate}{Validation mode.}
}

\value{\code{character} names.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




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
