\name{modelFrame}
\alias{modelFrame}
\title{Recursive dispatch of multilevel series functions}
\description{This function recursively dispatches user-defined stage functions
on multilevel long-format series and can optionally apply a
terminal fitter specified by 'form'.}
\usage{modelFrame(rd, fn = list("rtimes", 
    "scacum", "amod"), 
    lv = list(2, 1, 1), 
    form = "tdForm", 
    validate = c("none", 
        "warn", "stop"), 
    ...)}
\arguments{
  \item{rd}{\code{data.frame} or \code{list}. Dendroclimatic
chronology or multilevel ecological data series.}
  \item{fn}{\code{list}. Names of
recursive stage functions to
dispatch through
\code{\link{ringApply}}.
Stage functions must return a
\code{data.frame}. The
preferred grammar keeps
terminal fitting out of
\code{fn}; however, a final
\code{\link{frametoLme}} or
\code{\link{frametoFriedman}}
entry is still recognized for
backward compatibility.}
  \item{lv}{\code{list}. Splitting levels for the
functions that are evaluated through
\code{\link{ringApply}}. Numeric values are
resolved against the current hierarchy at each
stage. Character values are interpreted as
grouping columns in the current data. If a
terminal fitter is supplied as the last entry of
\code{fn} for backward compatibility,
\code{lv} applies only to the preceding stage
functions.}
  \item{form}{\code{NULL}, \code{character},
\code{formula}, or \code{list}. Terminal model
specification. \code{NULL} returns only the
processed long data. \code{\link{tdForm}},
\code{\link{lmeForm}}, a formula, or a
formula-like character string trigger
\code{\link{frametoLme}}. \code{\link{FriedmanForm}}
or an object inheriting from class
\code{"FriedmanForm"} trigger
\code{\link{frametoFriedman}}.}
  \item{validate}{\code{character}. Validation mode.}
  \item{\dots}{Further arguments in \code{\link{mUnits}}, the
recursive stage functions, the terminal fitter, or the
terminal specification helper named in \code{form}.}
}
\details{By default the workflow applies
\code{\link{rtimes}},
\code{\link{scacum}}, and
\code{\link{amod}} through
\code{\link{ringApply}}, then uses
\code{\link{tdForm}} as the terminal model
specification for \code{\link{frametoLme}}.
More generally, \code{modelFrame} behaves as a
recursive dispatcher for multilevel ecological
series. Functions listed in \code{fn} are
evaluated stage by stage on the hierarchy
defined by \code{lv}. The terminal path is now
driven by \code{form}: \code{NULL} returns the
processed long-format \code{data.frame};
\code{\link{tdForm}}, \code{\link{lmeForm}},
an explicit formula, or a formula-like
character string trigger
\code{\link{frametoLme}}; and
\code{\link{FriedmanForm}} triggers
\code{\link{frametoFriedman}}. For backward
compatibility, a terminal fitter placed as the
last entry of \code{fn} is still recognized,
but the preferred grammar is now to keep
\code{fn} for recursive stage functions and
\code{form} for the terminal model or smoother
specification. The implementation is
conservative with multilevel data: it prefers
explicit time columns over row names, reshapes
only when the input clearly looks wide,
restricts unit conversion to record-like
numeric columns, resolves numeric
\code{lv} positions against the current
hierarchy at each stage, and validates the
stage contract. In the terminal
\code{\link{frametoLme}} fit, rows with a
missing value in the fitted variables or the
grouping columns are dropped without a message;
with the default \code{\link{tdForm}} these are
the rows where \code{\link{rtimes}} gives no
time (16 of the 1008 rows of \code{Pchron}).
Arguments for \code{\link[nlme]{lme}}, such as
\code{random = pdDiag(~ log(csx))} with a
formula grouped by tree (a tree intercept and a
tree-level \code{log(csx)} slope, as in the
\code{\link{frametoLme}} example), are passed on
to \code{\link{frametoLme}}. Arguments in
\code{modelFrame} objects can be updated with
\code{\link{update}}. Use
\code{validate = "warn"} or
\code{validate = "stop"} for stricter checks.}
\value{Processed long-format \code{data.frame} if \code{form = NULL};
otherwise the object returned by the terminal fitter, with the
originating \code{modelFrame} call attached and class
\code{\"modelFrame\"} prepended.}
\references{Lara W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## ring-width chronologies (mm) of eight cores in two plots, one
## column per core
data(Pchron, envir = environment())
## allometric parameters: diameter inside bark (cm) to diameter at
## breast height (cm), then to total tree biomass (kg)
biom_param <- c(2.87, 0.85, 0.05, 2.5)

## rtimes, scacum and amod stage by stage, then a model on year with a
## random intercept per tree: these 2 plots x 2 trees x 2 cores support
## variation among trees only (tree labels are unique across plots here)
Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               form = 'log(x) ~ log(csx) + year | tree',
               random = pdDiag(~ 1))

## the same stages with a Friedman smoother as the terminal step
Pchron |>
    modelFrame(form = 'FriedmanForm', to = 'cm',
               MoreArgs = list(mp = c(2, 1, biom_param)),
               response = 'x', time = 'year')

## only the processed long data
Pchron |>
    modelFrame(form = NULL, to = 'cm',
               MoreArgs = list(mp = c(2, 1, biom_param))) |>
    as.data.frame() |>
    head()
}
