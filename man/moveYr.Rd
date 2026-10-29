\name{moveYr}
\alias{moveYr}
\title{Seasonal years}
\description{Monthly records in time-series replicates (usually of climate) are
labeled for the years can begin in a month other than January.}
\usage{moveYr(cd, ini.mnt = "Oct", 
    label_year = c("end", 
        "start"), keep_hydro_id = TRUE, 
    hydro_id_name = "hydro_year_id", 
    validate = c("none", 
        "warn", "stop"))}
\arguments{
  \item{cd}{\code{data.frame} of monthly records with the columns
\code{year} and \code{month}, and any grouping or value
columns; or a \code{numeric} vector of years named by month.}
  \item{ini.mnt}{\code{character} or \code{numeric}. First
month of the hydrological year, as a month
abbreviation, full month name or number from 1 to
12.}
  \item{label_year}{\code{character}. How
hydrological years are labeled
in the column \code{year}.
\code{'end'} (default) labels each
year by
the calendar year in which it
ends; \code{'start'} labels it by
the calendar year in which it
starts. The column
\code{hydro_year_id} always holds
the start year.}
  \item{keep_hydro_id}{\code{logical}. Add a column with the
calendar year in which each hydrological
year starts (data frames only).}
  \item{hydro_id_name}{\code{character}. Name of that
column. Other functions of the
package, such as \code{\link{wlai}},
find it only under the default
name.}
  \item{validate}{\code{character}. What to do
when month labels cannot be
read, years are not whole
numbers, several month columns
are found, or the rows are not
sorted by group, year and month:
\code{'none'} (default) goes on
silently (unreadable months
become \code{NA}), \code{'warn'}
warns and \code{'stop'} stops.}
}
\details{\code{character} months as defined in
\code{\link{month.abb}} or \code{\link{month.name}}.
The columns are found by the package's column
names: \code{year}, and \code{month} (month
abbreviations, full month names, or integers
1:12); a factor whose levels are month names is
also accepted as the month column.}
\value{For a data frame, the input with the month column as a factor of
month abbreviations, the column \code{year} relabeled by
hydrological year (see \code{label_year}), and, by default, a column
\code{hydro_year_id} with the start year of each hydrological year.
The calendar year is not kept: with \code{ini.mnt} = 'Oct' and
\code{label_year} = 'end', October to December 1955 are labeled 1956. It
can be recovered as \code{hydro_year_id} for the months from
\code{ini.mnt} to December and \code{hydro_year_id + 1} for the months
before \code{ini.mnt}.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## monthly precipitation and temperature of two plots (PTclim05)
data(PTclim05, envir = environment())
## hydrological years from October to September, labeled by the
## calendar year in which they end (label_year = 'end', the default)
cl <- PTclim05 |> moveYr()
head(cl)
## October to December 1955 belong to the hydrological year 1956
subset(cl, plot == 'P44005' & hydro_year_id == 1955 &
           month \%in\% c('Oct', 'Nov', 'Dec', 'Jan'))
## the calendar year is recovered from hydro_year_id (see Value)
cl$calendar_year <- cl$hydro_year_id +
    (match(as.character(cl$month), month.abb) < 10)
all(cl$calendar_year[order(cl$plot, cl$calendar_year, cl$month)] ==
    PTclim05$year[order(PTclim05$plot, PTclim05$year, PTclim05$month)])
## labels by the start year, and hydrological years from April
PTclim05 |> moveYr(label_year = 'start') |> head(3)
PTclim05 |> moveYr(ini.mnt = 'Apr') |> head(3)
}
