\name{normalizeGroupLevels}
\alias{normalizeGroupLevels}
\title{Order grouping levels}
\description{Grouping columns of processed data or of fitted objects are
converted to factors whose levels follow numeric order (7, 17, 18,
56) instead of the order of first appearance, or alphabetical order
for labels that are not numbers.}
\usage{normalizeGroupLevels(x, 
    groups = NULL, time = NULL, 
    keep_time_in_groups = NULL, 
    update_meta = TRUE, 
    drop_unused = TRUE)}
\arguments{
  \item{x}{A \code{data.frame}, such as the output of
\code{\link{ringApply}}, or a fitted object with a
\code{$fluc} residual series, such as those from
\code{\link{frametoLme}}, \code{\link{modelFrame}} or
\code{\link{frametoFriedman}}.}
  \item{groups}{\code{character} or \code{NULL}. Grouping
columns to order, as a vector such as
\code{c('plot', 'tree')} or a nested string such as
\code{'plot/tree/sample'}. If \code{NULL}, the
columns are read from the metadata.}
  \item{time}{\code{character} or \code{NULL}. The active
temporal column. If \code{NULL}, it is read from the
metadata.}
  \item{keep_time_in_groups}{\code{logical} or \code{NULL}. If
\code{TRUE}, the temporal column may stay
among the grouping columns; if
\code{FALSE}, it is removed. If
\code{NULL}, the metadata decide, and it
is removed by default.}
  \item{update_meta}{\code{logical}. Record the grouping
columns in the \code{BIOdry_meta} attribute of
the output.}
  \item{drop_unused}{\code{logical}. Drop unused factor levels.}
}
\details{The grouping columns are those named
in the second argument or, if it is not given,
those recorded in the \code{BIOdry_meta}
attribute, as set by
\code{\link{defineDataRoles}} or
\code{\link{ringApply}}. Fitted objects, such as
those from \code{\link{frametoLme}} or
\code{\link{modelFrame}}, carry no such metadata
themselves: the grouping columns are then read
from the metadata of their residual series
(\code{$fluc}), else from the grouping formula of
the fitted model, and only \code{$fluc} is
ordered; the model itself is unchanged. The
active temporal column is not treated as a
grouping column unless requested.

A column is ordered as numbers only when all its
non-missing labels read as numbers; its levels
are then the labels themselves, ordered by their
value, so labels such as "07" are kept. Other
columns keep their labels, in alphabetical order.
Numeric columns become factors with sorted
levels. Values and rows are not changed. Ordered
levels give a natural panel order in trellis
plots.}
\value{An object of the same kind as the input: a \code{data.frame} with
ordered grouping columns and its attributes kept, or the fitted
object with an ordered \code{$fluc}. If no data frame is found, the
input is returned unchanged with a warning.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03);
## Zeide model with ln(time) and a tree-level random part, as in the
## frametoLme example
data(Prings05, envir = environment())
data(Pradii03, envir = environment())
fit <- Prings05 |>
    ringApply(lv = 2, fn = 'rtimes') |>
    ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
              fn = 'scacum') |>
    frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
               random = pdDiag(~ log(csx)))
## tree levels of the residual series, as nlme orders them
levels(fit$fluc$tree)
## in numeric order
fit_n <- normalizeGroupLevels(fit)
levels(fit_n$fluc$tree)
## a data frame, with the grouping columns named
dfm <- Prings05 |>
    ringApply(lv = 2, fn = 'rtimes') |>
    normalizeGroupLevels(groups = 'plot/tree/sample')
lapply(dfm[c('plot', 'tree', 'sample')], levels)
}
