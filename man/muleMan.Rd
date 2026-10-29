\name{muleMan}
\alias{muleMan}
\title{Multilevel dendroclimatic correlograms}
\description{Multivariate correlograms between TRW fluctuations and climatic
fluctuations.}
\usage{muleMan(rd, cd, rd.var = NULL, 
    cd.var = NULL, ...)}
\arguments{
  \item{rd}{\code{dataframe} or \code{\link[nlme]{groupedData}}. TRW
fluctuations such as that produced by
\code{\link{modelFrame}}.}
  \item{cd}{\code{dataframe} or
\code{\link[nlme]{groupedData}}. Aridity-index fluctuations such
as that produced by \code{\link{modelFrame}}.}
  \item{rd.var}{\code{character} or \code{NULL}. Column name of
the TRW fluctuations to be compared. If
\code{NULL} then the first column is processed.}
  \item{cd.var}{\code{character} or \code{NULL}. Column name of
the aridity-index fluctuations to be compared. If
\code{NULL} then the first column is used.}
  \item{\dots}{Further arguments in \code{\link[ecodist]{mgram}}}
}
\details{Function \code{\link[ecodist]{mgram}}
is implemented to compare the dendroclimatic
fluctuations. Models being compared should have
common higher-level factors (see example).}
\value{\code{data.frame} object of multivariate correlations.}
\references{Lara W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## ring-width chronologies (mm) of eight cores and the allometric
## parameters of the modelFrame example
data(Pchron, envir = environment())
biom_param <- c(2.87, 0.85, 0.05, 2.5)
## tree-biomass fluctuations
trwf <- Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               log.t = FALSE, on.time = FALSE)
## aridity-index fluctuations; hydrological years labeled by the year
## in which they end
data(Prec, envir = environment())
data(Temp, envir = environment())
aif <- list(Prec, Temp) |>
    modelFrame(fn = list('moveYr', 'wlai'), lv = list('year', 'year'),
               form = 'lmeForm')
## Mantel correlograms between the two, for each core (100
## permutations to keep the example short)
set.seed(1)
mcomp <- muleMan(trwf, aif, nperm = 100)
head(mcomp$mmgram)
}
