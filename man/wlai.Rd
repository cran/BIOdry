\name{wlai}
\alias{wlai}
\title{Walter-Lieth aridity index}
\description{Computing the annual aridity index from Walter-Lieth climate diagrams}
\usage{wlai(cd, sqt = TRUE, 
    ini.mnt = NULL, label_year = c("end", 
        "start"), validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{cd}{\code{data.frame}. Multilevel climatic data series of
monthly precipitation sums (mm), and monthly average
temperatures (degree C). Precipitation is the column
\code{pr} and temperature the column \code{tm}; inputs declared
with \code{\link{defineDataRoles}} take precedence: declared
\code{pr} and \code{tm} are used by name, other declared
inputs in their order, precipitation first. If \code{pr} or
\code{tm} is missing, the first two numeric columns are used
and a warning names them. The month column is found by the
package's column name \code{month} (a factor whose levels are
month names is also accepted), and the year column by
\code{year}, or \code{hydro_year_id} as returned by
\code{\link{moveYr}}; without them a single whole-number
column is used as the year. If none of \code{year},
\code{hydro_year_id}, \code{month} or \code{time} is present
(and the row names are not month names), a warning is given.}
  \item{sqt}{\code{logical}. Print the square root of the
aridity index.}
  \item{ini.mnt}{\code{character} or \code{numeric}. Optional
start month to define annual blocks in long
climatic data. When supplied, yearly labels are
shifted before computing aridity indexes. Without
\code{ini.mnt}, calendar blocks start at the first
month of the earliest year; if the data have no
year column, the month of the first row is used,
so rows should then be in time order.}
  \item{label_year}{\code{character}. How
hydrological years are labeled
in the output column
\code{year}. \code{'end'}
(default) labels each year by the
calendar
year in which it ends, so October
2000 to September 2001 is labeled
2001, the year of ring formation;
\code{'start'} labels it by the
calendar year in which it starts.
It applies when
\code{ini.mnt} is supplied or when
\code{cd} already has a
\code{hydro_year_id} column, as
returned by \code{moveYr}. The
output column \code{hydro_year_id}
always holds the start year. If
the \code{year} column of \code{cd}
follows the other convention, a
warning is given and
\code{label_year} decides. Without
hydrological years the years are
calendar years and
\code{label_year} has no effect.}
  \item{validate}{\code{character}.
Validation mode.}
}

\value{A \code{data.frame} of class \code{wlai} with one row per annual
block: the aridity index \code{AI}, the year columns
(\code{hydro_year_id} and \code{year} when the input comes from
\code{\link{moveYr}}) and the grouping columns. The Walter-Lieth
geometry of each block is kept in attributes and drawn by
\code{\link{plot.wlai}}; no plot is drawn here.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## monthly precipitation (pr, mm) and temperature (tm, degrees C) of
## two plots, 1955-2005 (PTclim05)
data(PTclim05, envir = environment())
## aridity index of one calendar year of one plot
subset(PTclim05, plot == 'P44005' & year == 2000) |> wlai()
## one index per plot and hydrological year (October to September),
## labeled by the year in which it ends; the first and last
## hydrological years are incomplete and get NA
ai <- PTclim05 |>
    moveYr() |>
    wlai()
head(ai)
subset(ai, is.na(AI))
## the Walter-Lieth diagram of one block (see plot.wlai)
plot(ai, subset = list(plot = 'P44005', year = 2001))
}
