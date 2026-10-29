\name{slitFrame}
\alias{slitFrame}
\title{slitFrame}
\description{Robust multilevel splitting}
\usage{slitFrame(rd, lv = NULL, 
    validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{rd}{data.frame with grouping columns.}
  \item{lv}{NULL, character, or numeric selector for splitting columns.}
  \item{validate}{validation mode.}
}

\value{list of data.frames.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
data(Prings05, envir = environment())
data(PTclim05, envir = environment())
spl <- slitFrame(Prings05)
str(spl)
spl <- slitFrame(Prings05, "year")
str(spl)
spl <- slitFrame(PTclim05, c("plot", "year"))
str(spl)
}
