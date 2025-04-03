\name{slitFrame}
\alias{slitFrame}
\title{Multilevel splitting}
\description{This function splits a Multilevel data frame into factor levels.}
\usage{slitFrame(rd, lv = cClass(rd, 
    "factor"))}
\arguments{
  \item{rd}{\code{data.frame} object with factor-level columns.}
  \item{lv}{\code{Numeric} or
\code{character}. Position number in
the factor-level columns of \code{rd},
or correspondant column name to split
the data. If the spliting column is not
a factor, the character name of the
column should be used.}
}

\value{\code{list} of \code{data.frame} objects.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
##Ring data frame:
##Multilevel data frame of tree-ring widths:
data(Prings05, envir = environment())
data(PTclim05, envir = environment())
## split multilevel data into its second factor-level column:
spl <- slitFrame(Prings05)
str(spl)
## split the data into the factor-level: 'year':
spl <- slitFrame(Prings05,'year')
str(spl)
spl <- slitFrame(PTclim05,'year')
str(spl)

}
