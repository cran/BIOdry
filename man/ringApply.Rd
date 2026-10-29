\name{ringApply}
\alias{ringApply}
\title{Multilevel apply}
\description{Wrapper of \code{\link{Map}} to apply functions on multilevel data
series and preserve factor-level structure in the outputs.}
\usage{ringApply(rd, lv = 1, 
    fn = "scacum", focus = NULL, 
    target = NULL, focus_mode = c("auto", 
        "vector", "args", 
        "data.frame"), 
    validate = c("none", 
        "warn", "stop"), 
    ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{lv}{\code{numeric} position, or \code{character} name, of an
ecological factor in the processed MEDS.}
  \item{fn}{\code{character} name of the function to be
evaluated (see details). Default 'scacum'
computes scaled-cumulative radii.}
  \item{focus}{Optional \code{focus} column or columns. Use
\code{NULL} to keep the historical full-data
dispatch for core BIOdry helpers and otherwise
default to the active response stored in
\code{BIOdry_meta}. Use \code{".data"} to
force full-data-frame dispatch.}
  \item{target}{Optional output column name used when a
focused helper returns a single vector of the
same length as the split block. By default the
focused column is replaced when a single \code{focus}
column is used.}
  \item{focus_mode}{focus_mode }
  \item{validate}{Validation mode. Use
\code{"none"} for quiet backward-compatible behavior,
\code{"warn"} for diagnostics, or \code{"stop"} for
strict checking.}
  \item{\dots}{Further arguments in the function being specified
\code{fn} argument (see details)}
}
\details{Other functions such as
\code{\link{rtimes}}, \code{\link{scacum}},
\code{\link{amod}}, or \code{\link{wlai}} can
be implemented. Function arguments should be
formulated as suggested in
\code{\link{mapply}}, with constant
arguments being stored in a \code{MoreArgs}
list. This implementation keeps
\code{MoreArgs} constant across split blocks.
The split levels are the factor and character
columns other than the month column (see
\code{\link{cClass}}); whole-number columns such
as \code{year} are split levels only when
requested through \code{lv}. The nesting
structure is resolved
through \code{\link{slitFrame}}, which is the
infrastructure helper used to materialize the
hierarchy defined by \code{lv}. When
\code{lv} is supplied, whether as a numeric
position or as a character level name, that
choice controls the split hierarchy
explicitly.

After splitting and before applying
\code{fn}, each block is ordered on the time
variables that remain inside the block.
Variables used as split levels are excluded
from the ordering keys. Thus a call such as
\code{lv = 'year'} orders within yearly
blocks by finer time variables such as month,
day, or relative time rather than by year
again, whereas a call such as
\code{lv = 'sample'} orders within each ring
series by year and then by any finer time
variable. Ordering prefers
\code{hydro_year_id}, then year-like
variables not used for splitting, then
month-like variables, and finally relative
time variables such as \code{time},
\code{age}, or \code{cambial_age}. This
contract is important because other BIOdry
functions can rely on \code{ringApply} to
deliver already split and time-ordered
blocks. Split variables used internally for a
transformation step are recorded in the
metadata history, but they are not promoted
automatically to the persistent grouping
hierarchy used later by
\code{frametoLme} or
\code{frametoFriedman}. In particular, a
derived time key such as
\code{hydro_year_id} may be the active time
variable after \code{moveYr} while remaining
outside the grouping hierarchy unless the user
explicitly requested otherwise in
\code{defineDataRoles}.

Duplicated output column names are collapsed
when they are exact duplicates. Output
columns are ordered explicitly with record or
measured variables first (for example
\code{x}, \code{csx}, \code{pr},
\code{pm}, or \code{tm}), then time
variables (for example hour, day, week,
month, or year), and finally grouping
factors ordered from the lowest available
hierarchy to the broadest hierarchy
(for example \code{sample} before
\code{tree}, \code{plot}, \code{zone}, or
\code{biome}). By default, validation is
silent for backwards compatibility; use
\code{validate = "warn"} or
\code{validate = "stop"} for stricter
diagnostics.

This patched version also attaches a small
workflow attribute named
\code{"BIOdry_meta"} to the returned data
frame. The attribute stores lightweight
context from the current evaluation, such as
the applied function name, the split levels,
a compact summary of relevant arguments,
simple role hints (response, time, grouping
variables), and a conservative variable-name
map when a known transformation creates a new
key, for example \code{year ->
hydro_year_id} after \code{moveYr}. The role
hints are taken from column types and record
context only: they do not override the type
rule in \code{lmeForm}, \code{tdForm},
\code{frametoLme} or \code{FriedmanForm}; only
roles declared with \code{\link{defineDataRoles}}
do. The attribute does not alter the returned
columns.

When constructor metadata or prior

\code{ringApply} steps indicate that a
time variable has been replaced by a
derived annual key, character values of

\code{lv} are resolved against that
current key before splitting. Thus a
workflow such as

\code{moveYr -> wlai} can keep using

\code{lv = 'year'} while internally
splitting on 
\code{hydro_year_id} when
that derived variable has become the
active annual index. The original
calendar 
\code{year} column is retained
for reference when the applied function
returns it.

For climatic long-format data, a safer
workflow is often to first use
\code{\link{moveYr}} to create a stable annual
key such as \code{hydro_year_id}, then split
by the relevant factor columns plus that year
key.}
\value{\code{data.frame} object preserving initial factor-level columns.}
\references{Lara, W., F. Bravo,
D. Maguire. 2013. Modeling patterns between
drought and tree biomass growth from
dendrochronological data: A multilevel
approach. Agric. For. Meteorol.,
178-179:140-151.}
\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{

##Multilevel ecological data series (MEDS) of tree-ring widths:
data(Prings05,envir = environment())
## Radial increments measured on 2003:
data(Pradii03,envir = environment())    
## MEDS of monthly precipitation sums and average temperatures:
data(PTclim05,envir = environment())

##Tree-level scaling of years of formation
##with 'rtimes' function:
dfm1 <- ringApply(Prings05,
                  lv = 2,
                  fn = 'rtimes')
str(dfm1)
##Relative time-units from year 1 to year 9:
subset(dfm1,time\%in\%c(1:9,NA))

## Sample-level scaling of TRW chronologies around reference radii
## which were measured at 2003:
dfm2 <- ringApply(dfm1,
                  lv = 'sample',
                  sc.c = Pradii03,
                  rf.t = 2003,
                  fn = 'scacum')
str(dfm2)    
##Sample-level modeling of basal areas (mm2) via allometric
##scaling:
dfm3 <- ringApply(dfm2,
                  lv = 'sample',
                  fn = 'amod',
                  MoreArgs = list(mp = c(2,1,0.25 * pi,2)))
str(dfm3)

## Seasonal years from 'October' to 'September':
cl1 <- ringApply(PTclim05,
                 lv = 'year',
                 fn = 'moveYr')
tail(cl1,15)
attr(cl1, 'BIOdry_meta')

##Year-level aridity indexes: 
wl <- ringApply(cl1,
                lv = 'year',
                fn = 'wlai')
str(wl)
attr(wl, 'BIOdry_meta')

## Sample-level unit conversion on the active response:
xu <- ringApply(Prings05,
                lv = 'sample',
                fn = 'mUnits',
                from = 'mm',
                to = 'cm')
head(xu)

## Plot of aridity-index fluctuations:
d <- groupedData(lmeForm(wl),wl)
plot(d)        

}
