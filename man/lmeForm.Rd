\name{lmeForm}
\alias{lmeForm}
\title{LME formula}
\description{This function computes LME formulas from multilevel ecological
data series (MEDS).}
\usage{lmeForm(rd, prim.cov = FALSE, 
    resp = NULL, covar = NULL, 
    lev.rm = NULL)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series}
  \item{prim.cov}{\code{Logical}: should the LME formula only
be printed in primary covariate form: '~ cov'?
If FALSE then a complete form: 'resp ~ \code{covar} |
group' is formulated.}
  \item{resp}{\code{NULL} or \code{character}. Column name of
the response. If NULL then the name of the first
numeric column of the MEDS is used.}
  \item{covar}{\code{NULL} or \code{character}. Column name(s)
of the covariate(s). If \code{NULL} then the name
of the first time-unit column in the MEDS is used.}
  \item{lev.rm}{\code{NULL}, \code{character} or \code{numeric}
vector of levels in the MEDS to be removed from
the groups.}
}
\details{Formulas of the form \code{resp ~ cov
| group} (see \code{\link[nlme]{groupedData}}
function) are computed from MEDS. The formulas
can be implemented by
\code{\link{modelFrame}} function to detrend MEDS}
\value{\code{formula} with any of the forms: \code{resp ~ cov | group} or
\code{~ cov}.}
\references{Pinheiro J. C.,
D. M. Bates. 2000. Mixed-effects models in S
and S-PLUS. Springer, New York.}
\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
##Multilevel ecological data series of tree-ring widths:
data(Prings05,envir = environment())

## LME formula:
form1 <- lmeForm(Prings05,prim.cov = FALSE)
print(form1)
## removing the sample level from the formula
form2 <- lmeForm(Prings05,lev.rm = 'sample')
form2 <- lmeForm(Prings05,lev.rm = 1)

## groupedData object with the LME formula 
gdata <- groupedData(lmeForm(Prings05,lev.rm = 1),
                     data = Prings05)
plot(gdata,groups = ~ sample)
}
