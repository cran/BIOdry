\name{lmeForm}
\alias{lmeForm}
\title{LME formula}
\description{LME formula with grouping levels being defined by factor-level
columns in a data frame.}
\usage{lmeForm(rd, prim.cov = FALSE, resp = NULL, covar = NULL, lev.rm = NULL)}
\arguments{
  \item{rd}{\code{data.frame} object with fator-level columns.}
  \item{prim.cov}{\code{Logical}: should the formula be
printed in the form of primary covariate: '~
cov'?  If FALSE then a complete form: 'resp
~ covar | group' is formulated.}
  \item{resp}{\code{NULL} or \code{character}. Column name of
the response. If NULL name of the first numeric
column of the data frame is used.}
  \item{covar}{\code{NULL} or \code{character}. Column name(s)
of the covariate(s). If \code{NULL} name of the
first temporal column in the data frame is used.}
  \item{lev.rm}{\code{NULL}, \code{character} or \code{numeric}
vector of levels in the data frame to be removed
from the groups.}
}

\value{\code{formula} with any of the forms: \code{resp ~ cov | group} or
\code{~ cov}.}
\references{Pinheiro J. C., D. M. Bates. 2000. Mixed-effects models in S and S-PLUS. Springer, New York.                     }
\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel data frame of tree-ring widths:
data(Prings05,,envir = environment())

 ## groupedData object with  lmeForm 
gdata <- groupedData(lmeForm(Prings05,lev.rm = 1),data = Prings05)
plot(gdata,groups = ~ sample)

## LME formula in form of covariate:
form1 <- lmeForm(Prings05,prim.cov = FALSE)
print(form1)

}
