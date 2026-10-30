\name{summary.modelFrame}
\alias{summary.modelFrame}
\title{Summarize modelFrame objects}
\description{A compact summary of a \code{\link{modelFrame}} \code{object} is obtained.}
\usage{\method{summary}{modelFrame}(object, 
    ...)}
\arguments{
  \item{object}{Object inheriting from class \code{\link{modelFrame}}.}
  \item{\dots}{Additional arguments passed to the terminal summary method.}
}
\details{This method wraps the terminal
fitter stored in the \code{\link{modelFrame}}
\code{object}. If \code{form = NULL}, the method
returns a compact description of the processed
long-format data and its grouped plotting
structure.}
\value{A compact summary \code{object} for \code{\link{modelFrame}}.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
data(Pchron, envir = environment())
biom_param <- c(2.87, 0.85, 0.05, 2.5)
## the biomass model on year with a random intercept per tree
s <- Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               form = 'log(x) ~ log(csx) + year | tree',
               random = pdDiag(~ 1)) |>
    summary()
## fit statistics and fixed effects of the terminal model
s$AIC
s$fixed
## with form = NULL the summary describes the processed data
Pchron |>
    modelFrame(form = NULL, to = 'cm',
               MoreArgs = list(mp = c(2, 1, biom_param))) |>
    summary()
}
