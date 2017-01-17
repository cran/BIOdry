\name{shiftFrame}
\alias{shiftFrame}
\title{MEDS formatting}
\description{Ring-data frames (e.g. \code{dplR} chronologies) are formatted into
multilevel ecological data series (or vice versa). }
\usage{shiftFrame(rd, lev.nm = c("plot", "tree", "sample"), which.x = NULL, 
    un = NULL)}
\arguments{
  \item{rd}{\code{data.frame}. Ring-data frame (see details), or
multilevel ecological data series (MEDS).}
  \item{lev.nm}{for the case of ring-data
frames, \code{character}
vector with names of the
factor-level columns in the
final MEDS, beginning with
name of the highest level
column and ending with name
of the lowest level
column. If \code{rd} is a
MEDS then this argument is
ignored.}
  \item{which.x}{for the case of MEDS, \code{character} name of
the column to be reshaped into a ring-data
frame. If NULL then the first \code{numeric}
column is processed. If \code{rd} is a ring-data
frame then this argument is ignored.}
  \item{un}{\code{NULL}, one, or two \code{character} names of
SI units to record/transform the processed
variables. One character records metric system; two
characters with the form c(initial, final) change SI
units in the processed data. Defined SI units are
micrometers 'mmm', milimeters 'mm', centimeters 'cm',
decimeters 'dm', or meters 'm'. If NULL then no metric
system is recorded.}
}
\details{Correct formatting of ring-data
frames requires their row names to be
time-units labels (e.g. years). The column
names should be dot-separated labels
representing the hierarchy of ecological
factors, where the higher levels are defined
first and the lower levels after. For
example, code 'P16106.17' is the column name
of tree '17' in plot 'P16106'. SI units of
the processed chronologies can also be
changed.}
\value{If \code{rd} is a ring-data frame then output is a MEDS. If
\code{rd} is a MEDS then the output is a ring-data frame (see
details).}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##tree-ring MEDS:
data(Prings05,envir = environment())

## Formatting the MEDS into a ring-data frame:
pwide <- shiftFrame(Prings05)
str(pwide)
## Formatting the ring-data frame into a MEDS, and changing SI
## units of the rings from milimeters to micrometers:
plong <- shiftFrame(pwide,un = c('mm','mmm'))
str(plong)
}
