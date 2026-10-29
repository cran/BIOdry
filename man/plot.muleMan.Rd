\name{plot.muleMan}
\alias{plot.muleMan}
\title{Plot muleMan objects}
\description{Diagnostic Trellis plot for fluctuations in
\code{\link{muleMan}} objects are obtained.}
\usage{\method{plot}{muleMan}(x, ...)}
\arguments{
  \item{x}{An object inheriting from class \code{\link{muleMan}}.}
  \item{\dots}{further arguments passed to the Trellis plot function.}
}
\details{One panel is drawn for each
correlogram. Panels are ordered by the grouping
levels from the outermost to the innermost (plot,
then tree, then core in the bundled data);
numeric labels are ordered as numbers. Filled
points mark lags with p-values below 0.05. The
axes are labeled "Lag" and "Mantel r" unless
\code{xlab} or \code{ylab} are given.}


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
## one correlogram per core; filled points mark significant lags
plot(mcomp)
}
