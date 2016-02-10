\name{shiftFrame}
\alias{shiftFrame}
\title{Shifting of ring-data frames}
\description{Ring-data frames are reshaped into multilevel data frames (or vice
versa). SI units of the processed chronologies can be changed.}
\usage{shiftFrame(rd, lev.nm = c("plot", "tree", "sample"), which.x = NULL, 
    un = NULL)}
\arguments{
  \item{rd}{\code{data.frame}. Ring-data frame (see details), or
multilevel data frame (see value).}
  \item{lev.nm}{for the case of ring-data
frames, \code{character}
vector with names of the
factor-level columns in the
final multilevel data
frame, beginning with name
of the highest level column
and ending with the name of
the lowest level column. If
\code{rd} is a multilevel
data frame then this
argument is ignored.}
  \item{which.x}{for the case of multilevel data frames,
\code{character} name of the column to be
reshaped into a ring-data frame. If NULL then
the first \code{numeric} column is processed. If
\code{rd} is a ring-data frame then this
argument is ignored.}
  \item{un}{\code{NULL}, one, or two \code{character} names of
SI units to record/transform the processed
variables. One character records metric system; two
characters with the form c(initial, final) change SI
units in the processed data. Defined SI units are
micrometers 'mmm', milimeters 'mm', centimeters 'cm',
decimeters 'dm', or meters 'm'. If NULL then no metric
system is recorded.}
}
\details{Rows of ring-data frames should be
named with chronological years, and columns
of such data frames should be labeled with
level units defined by sample design. Level
units in labels are separated with dot (.),
beginning with names of higher level units
(i.e. ecorregion, climatic location, or
plot) and ending with the lowest level unit
(usually sample/replicate). For example, the
code name of core 'a' in tree '2' on plot
'P16001' on ecorregion 'M1' will have the
name: 'M1.P16001.2.a'.}
\value{If \code{rd} is a ring-data frame then output is a multilevel data
frame with the reshaped variable in the first column and years on
the second one, followed by factor-level columns from the column
with the lowest level units (sample/replicate) to the column with
the higest possible level units. If \code{rd} is a multilevel data
frame then the output is a ring-data frame (see details).}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())

## Reshaping multilevel data into a ring-data frame:
pwide <- shiftFrame(Prings05)
str(pwide)
## Reshaping the ring-data frame into the initial multilevel data,
## and defining SI units:
plong <- shiftFrame(pwide,un = 'mmm')
str(plong)
}
