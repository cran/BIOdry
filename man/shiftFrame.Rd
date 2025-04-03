\name{shiftFrame}
\alias{shiftFrame}
\title{MEDS formatting}
\description{dendroclimatic chronologies (trw, and climatic data) are formatted
into multilevel ecological data series. SI units of continuous
variables in the data can be transformed.}
\usage{shiftFrame(rd, f.nm = NULL, 
    x.nm = names(rd)[1L], 
    t.nm = "year", ...)}
\arguments{
  \item{rd}{\code{data.frame} or \code{list}. Dendroclimatic
chronology DC (see details) or list of two DCs
(e.g. precipitation and temperature records), or multilevel
ecological data series (MEDS).}
  \item{f.nm}{\code{character} vector. In the case of formatting
ring-data frames, column names of the factors in
the new MEDS. If \code{NULL} then this argument is
recycled from attributes in \code{rd}. If such an
attribute is also \code{NULL} then a sequence of
codes (F1, F2, \code{...}, Fn) is used.}
  \item{x.nm}{\code{character}. In the case of
formatting MEDS, name of the variable to be
reshaped. Default uses name of first
variable of \code{rd}.}
  \item{t.nm}{\code{character}.In the case of formatting MEDS,
name of the time-units variable .}
  \item{\dots}{Further arguments in \code{\link{mUnits}}.}
}
\details{Row names of dendroclimatic data
frames are time units (e.g. years). Column
names are dot-separated labels representing
the hierarchy of ecological or time-units
factors, where the higher levels are defined
first and the lower levels after. For
example, code 'P16106.17' is the column name
of core 'a' in tree '17' in plot
'P16106'. Labels containing monthly
abbreviations are also formatted.}
\value{When \code{rd} argument is a dendroclimatic chronology (see
details) then the output is a \code{\link[nlme]{groupedData}} object,
and viceversa.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
##tree-ring widths formated as a \\code{\\link[nlme]{groupedData}} object:
data(Prings05,envir = environment())

## Formatting the \\code{\\link[nlme]{groupedData}} object into a ring-data frame:
pwide <- shiftFrame(Prings05, from = 'mm', to = 'mmm')
str(pwide)
## Formatting the ring-data frame into a groupedData object, and
## changing SI units from micrometers to milimeters:
plong <- shiftFrame(pwide,from = 'mmm', to = 'mm')
plot(plong)
}
