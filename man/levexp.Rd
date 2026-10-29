\name{levexp}
\alias{levexp}
\title{levexp}
\description{Fast and compatibility-oriented vector releveling
Expands or reduces a named numeric vector by matching its level names
to split labels from multilevel ecological data.}
\usage{levexp(x, levels, validate = c("none", 
    "warn", "stop"))}
\arguments{
  \item{x}{named numeric vector with level names to be matched.}
  \item{levels}{data.frame, split list, or character vector of split names.}
  \item{validate}{validation mode.}
}

\value{numeric vector with expanded/reduced \code{levels}.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
data(Prings05, envir = environment())
data(Pradii03, envir = environment())

refs <- levexp(Pradii03, Prings05)
refs
}
