\name{print.modelFrame}
\alias{print.modelFrame}
\title{Print modelFrame objects}
\description{A compact printed representation of a \code{\link{modelFrame}} object is obtained.}
\usage{\method{print}{modelFrame}(x, ...)}
\arguments{
  \item{x}{Object inheriting from class \code{\link{modelFrame}}.}
  \item{\dots}{Additional arguments passed to \code{\link{summary.modelFrame}}.}
}

\value{The compact printed representation.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
data(Pchron, envir = environment())
biom_param <- c(2.87, 0.85, 0.05, 2.5)
## printing shows a compact description, not the whole data
Pchron |>
    modelFrame(form = NULL, to = 'cm',
               MoreArgs = list(mp = c(2, 1, biom_param))) |>
    print()
}
