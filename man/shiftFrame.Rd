\name{shiftFrame}
\alias{shiftFrame}
\title{Shifting of ring-data frames}
\description{Ring-data frames are reshaped into multilevel data frames (or vice versa). SI units of the processed chronologies can be changed.}
\usage{shiftFrame(rd, which.x = NULL, un = NULL)}
\arguments{
  \item{rd}{\code{data.frame}. Ring-data frame (see details), or
multilevel data frame (see value).}
  \item{which.x}{for the case of multilevel data
frames, \code{character} name of the column to be
reshaped.If NULL then the first \code{numeric}
column is processed.}
  \item{un}{\code{NULL}, one, or two \code{character} units of
the metric system to record/transform the processed
variables. One character records metric system; two
characters with the form c(initial, final) change
units in processed data. Defined SI units are
micrometers 'mmm', milimeters 'mm', centimeters 'cm',
decimeters 'dm', or meters 'm'. If NULL then no metric
system is recorded.}
}
\details{Rows of the ring-data frames should be named with chronological years,and the columns should be labeled with the levels from sample design. Each of the levels is separated with dot (.), begining with highest level (i.e. ecorregion, climatic location, or plot)  and ending with the lowest level (usually core/replicate). For example, the code name of core 'a' in tree '2' on plot 'P16001' on ecorregion 'M1' will have the name: 'M1.P16001.2.a'. }
\value{If \code{rd} is a ring-data frame then output is a multilevel data frame with the reshaped variable in the first column and years on the second one, followed by factor-level columns from lowest level (core/replicate) to higest possible level. If \code{rd} is a multilevel data frame then the output is a ring-data frame (see details).}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
##Multilevel data frame of tree-ring widths:
data(Prings05,envir = environment())

## Reshaping multilevel data into a ring-data frame:
pwide <- shiftFrame(Prings05)
str(pwide)
## Reshaping the ring-data frame into initial multilevel data:
plong <- shiftFrame(pwide)
str(plong)
}
