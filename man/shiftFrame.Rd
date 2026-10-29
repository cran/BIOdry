\name{shiftFrame}
\alias{shiftFrame}
\title{shiftFrame}
\description{dendroclimatic chronologies (trw, and climatic data) are formatted
into multilevel ecological data series. SI units of continuous
variables in the data can be transformed.}
\usage{shiftFrame(rd, f.nm = NULL, 
    x.nm = names(rd)[1L], 
    t.nm = "year", split_pattern = "\\\.", 
    level_sep = ".", 
    ...)}
\arguments{
  \item{rd}{rd }
  \item{f.nm}{f.nm }
  \item{x.nm}{x.nm }
  \item{t.nm}{t.nm }
  \item{split_pattern}{split_pattern }
  \item{level_sep}{level_sep }
  \item{\dots}{\dots }
}
\details{Wide BIOdry data may represent either
tree-ring or climatic series. The patched
implementation prefers explicit time columns in
wide data, that is, the column named by
\code{t.nm}, whole-number numeric columns and the
\code{month} column, and then treats the remaining value
columns as hierarchy-encoded series labels. This
rule applies to both climate and tree-ring wide
data. Hierarchy levels are parsed from value-column
names with the configurable regular expression
given by \code{split_pattern}. The default keeps
the historical dot-separated BIOdry convention.

When long data are shifted to wide format, time
variables are retained as independent columns
rather than being hidden in row names. Value
columns are then rebuilt from the grouping
hierarchy using \code{level_sep}. When long
data contain more than one measured variable,
for example \code{tm} and \code{pm}, the wide
columns are prefixed by the measure name, such as
\code{tm.z1} or \code{pm.z1}, so the reverse
conversion can recover all value variables. This makes wide
to long round-trips safer for data with more than
one time variable, such as \code{year + month} in
climate series or \code{year + cambial_age} in
tree-ring series.

Row-name time is still accepted as a backward-
compatibility fallback for older BIOdry wide
chronologies. In that case, row names are used only
when explicit time columns are absent.

When a wide object produced by \code{shiftFrame()}
is shifted back to long format, the stored metadata
for \code{x.nm}, \code{f.nm}, \code{t.nm}, and
\code{time_cols} take precedence over the type rule
(see \code{\link{cClass}}).
This makes wide-to-long round-trips deterministic.
For single-response long results that are returned as
\code{groupedData}, the grouping formula is built from
the actual response variable and the stored hierarchy,
rather than being derived again from column types.
Long-format outputs are ordered explicitly as record
or measured variables first, then time variables,
and finally grouping factors from the lowest
resolved hierarchy level to the broadest one.}


\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
data(Prings05, envir = environment())
pwide <- shiftFrame(Prings05, from = 'mm', to = 'mmm')
str(pwide)
plong <- shiftFrame(pwide, from = 'mmm', to = 'mm')
plot(plong)
}
