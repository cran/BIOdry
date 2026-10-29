\name{lmeForm}
\alias{lmeForm}
\title{LME formula}
\description{This function computes LME formulas from multilevel ecological
data series (MEDS).}
\usage{lmeForm(rd, prim.cov = FALSE, 
    resp = NULL, covar = NULL, 
    lev.rm = NULL, validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series}
  \item{prim.cov}{\code{Logical}: should the LME formula only
be printed in primary covariate form: '~ cov'?
If FALSE then a complete form: 'resp ~ \code{covar} |
group' is formulated.}
  \item{resp}{\code{NULL} or \code{character}. Column name of
the response. If NULL then a response declared with
\code{defineDataRoles} is used, and otherwise the
first non-integer numeric column of the MEDS.}
  \item{covar}{\code{NULL} or \code{character}. Column name(s)
of the covariate(s). If \code{NULL} then a time
variable declared with \code{defineDataRoles} is
used, and otherwise the first whole-number numeric
column of the MEDS.}
  \item{lev.rm}{\code{NULL}, \code{character} or \code{numeric}
vector of levels in the MEDS to be removed from
the groups.}
  \item{validate}{validate }
}
\details{Formulas of the form \code{resp ~ cov
| group} (see \code{\link[nlme]{groupedData}}
function) are computed from MEDS. The formulas
can be implemented by
\code{\link{modelFrame}} function to detrend MEDS.

Column roles come from column types (see
\code{\link{cClass}}): the response is
the first non-integer numeric column, the
covariate is the first whole-number numeric
column, and the grouping levels are the factor
and character columns, outer level first (the
reverse of the column order). The month column
(named \code{month}, or a factor whose levels are
month names) is never a grouping level.

A response, time covariate or grouping levels
declared with \code{\link{defineDataRoles}} take
precedence. The metadata that
\code{\link{ringApply}} records automatically does
not override the type rule. A declared time
variable is removed from the grouping levels
unless the metadata sets
\code{time_in_groups = TRUE}. To use a
whole-number column such as \code{year} as a
grouping level, see \code{\link{frametoLme}}.}
\value{\code{formula} with any of the forms: \code{resp ~ cov | group} or
\code{~ cov}.}
\references{Pinheiro J. C.,
D. M. Bates. 2000. Mixed-effects models in S
and S-PLUS. Springer, New York.}
\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




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
