\name{print.summary.modelFrame}
\alias{print.summary.modelFrame}
\title{Print compact modelFrame summaries}
\description{A compact printed summary of a \code{\link{modelFrame}} object is obtained.}
\usage{\method{print}{summary.modelFrame}(x, 
    digits = max(3L, 
        getOption("digits") - 
            2L), ...)}
\arguments{
  \item{x}{Object inheriting from class \code{"summary.modelFrame"}.}
  \item{digits}{Integer number of \code{digits}.}
  \item{\dots}{Unused extra arguments.}
}

\value{The printed compact summary.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
data(Pchron, envir = environment())
biom_param <- c(2.87, 0.85, 0.05, 2.5)
Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               form = 'log(x) ~ log(csx) + year | tree',
               random = pdDiag(~ 1)) |>
    summary() |>
    print(digits = 3)
}
