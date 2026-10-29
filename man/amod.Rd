\name{amod}
\alias{amod}
\title{amod}
\description{Allometric scaling.
Allometric models and parameters are used to scale organic growth.}
\usage{amod(x, mp = c(1, 1), 
    fun = y ~ a * (x^b), 
    lv = NULL, validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{x}{\code{numeric} vector or
<<\code{data.frame}.}
  \item{mp}{\code{numeric}. Allometric
parameters. Several groups
may be supplied recursively.}
  \item{fun}{\code{formula}.}
  \item{lv}{\code{NULL}, \code{character},
<<or \code{numeric}. Grouping
level(s) to split multilevel
data frames before scaling.}
  \item{validate}{Validation mode.}
}
\details{. This patched version keeps the
original block-level helper behavior, but it
also works on multilevel data frames. When a
data frame is supplied with \code{lv}, the
function first resolves the grouping
hierarchy from \code{lv}, then uses
\code{slitFrame()} to materialize those
groups, orders rows within each block on the
available time variables that remain after
splitting, and finally applies the
allometric model to each ordered block.
Character and numeric \code{lv} values are
both supported. Numeric \code{lv} values are
interpreted as positions in the detected
nesting structure, whereas character values
are matched to grouping names directly.
Cumulative organic growth is taken from
\code{csx} when available. If \code{csx} is
absent but \code{x} or \code{trw} is
present, cumulative growth is derived with
\code{cumsum()} after time ordering only for
the internal scaling step, so the function
can operate on simpler long-format tree-ring
tables. When the allometric model is applied,
the returned object always contains both the
scaled increments (\code{x}) and the scaled
cumulative growth state (\code{csx}). This
keeps the transformed increment and state
variables paired explicitly after scaling.
The first transformed increment is the change
from the size before the first ring (\code{csx}
minus the first ring width, or zero when no ring
widths are available) to the first transformed
cumulative value, so identity settings such as
\code{mp = c(1,1)} preserve the original
increments rather than introducing an initial
\code{NA}. Direct use
on climatic tables is only appropriate when a
meaningful cumulative source is available or
can be derived before scaling. The returned
data frame preserves the original row order
after block-wise processing and keeps scaled
outcomes before time and grouping columns.}
\value{\code{data.frame} of the scaled variable (\code{x}) and cumulative
scaled growth (csx). For multilevel data frames, these are
computed within each ordered split block.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Simulating TRW records:
set.seed(1)
trw <- ts(abs(rnorm(12,1,1)),start = 1950)
## Cumulative TRW:
cri <- cumsum(trw)
## tree diameters
td <- amod(cri,mp = c(2,1))
## plot of the tree diameters and the
## relative increments:
plot(ts(td))
}
