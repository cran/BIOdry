\name{splitFrame}
\alias{splitFrame}
\title{Splitting of multilevel data}
\description{This function splits a Multilevel data frame into the specified level.}
\usage{splitFrame(rd, lv = 1)}
\arguments{
  \item{rd}{\code{data.frame} object with factor-level columns.}
  \item{lv}{\code{Numeric} or \code{character}. Position number in
the factor-level columns of \code{rd}, or correspondant
column name to split the data. If the spliting column is
not a factor, the character name of the column should be used.}
}

\value{\code{list} of \code{data.frame} objects.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Ring data frame:
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())

## split the multilevel data into its second factor-level column:
spl <- splitFrame(Prings05,2)
str(spl)
## split the data into the factor-level: 'year':
spl <- splitFrame(Prings05,'year')
str(spl)
}
