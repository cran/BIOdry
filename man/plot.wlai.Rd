\name{plot.wlai}
\alias{plot.wlai}
\title{Plot Walter-Lieth climate diagrams}
\description{A Walter-Lieth climate diagram of one annual block of a
\code{\link{wlai}} object is drawn, with its aridity index.}
\usage{\method{plot}{wlai}(x, y = NULL, 
    which = NULL, subset = NULL, 
    year = NULL, hydro_year_id = NULL, 
    hatch = TRUE, dry_density = 18, 
    moist_density = 18, 
    dry_angle = 45, moist_angle = -45, 
    ...)}
\arguments{
  \item{x}{A \code{\link{wlai}} object.}
  \item{y}{Not used; kept for the \code{plot()} generic.}
  \item{which}{\code{numeric} position or \code{character}
name of the block to draw.}
  \item{subset}{Named \code{list} (or one-row
\code{data.frame}) of column values of the
\code{wlai} table selecting the block, such as the
plot label and a period label.}
  \item{year}{Label of the \code{year} column of the block.}
  \item{hydro_year_id}{Label of the \code{hydro_year_id} column
of the block.}
  \item{hatch}{\code{logical}. Hatch the dry and moist seasons;
if \code{FALSE}, fill them in gray.}
  \item{dry_density}{\code{numeric}. Hatching density of the dry
season, in lines per inch.}
  \item{moist_density}{\code{numeric}. Hatching density of the
moist season, in lines per inch.}
  \item{dry_angle}{\code{numeric}. Hatching angle of the dry
season, in degrees.}
  \item{moist_angle}{\code{numeric}. Hatching angle of the moist
season, in degrees.}
  \item{\dots}{Further graphical parameters passed to
\code{\link[graphics]{plot.default}}.}
}
\details{Precipitation (right axis, mm) and
temperature (left axis, degrees C) are drawn on
the Walter-Lieth scale computed by
\code{\link{wlai}}: 10 degrees C match 20 mm
(P = 2T), and precipitation above 100 mm is
compressed. Months whose precipitation curve lies
below the temperature curve form the dry season;
the others form the moist season, and
precipitation above 100 mm is filled in blue. The
aridity index (AI) is printed above the diagram,
followed by the grouping labels and the
\code{year} label of the block; that label
follows the labeling chosen in \code{\link{moveYr}} and
\code{\link{wlai}}: P44005.2001 for October 2000
to September 2001 with the default \code{'end'}.

A \code{wlai} object computed on long-format data
(several plots or years) holds one annual block
per plot and period. Block names join the
grouping labels and the \code{hydro_year_id} of the block,
the calendar label of its first month: P44005.2000
holds October 2000 to September 2001. A block is
chosen by position or block name, by the labels
of the \code{year} or \code{hydro_year_id}
columns of the \code{wlai} table, or by a named
list of column values. Without a selection the
first block is drawn, with a warning; a selection
that matches several blocks stops with an error.}
\value{Called for its side effect, the diagram; \code{NULL} is returned
invisibly.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## monthly precipitation and temperature of plot P44005 (PTclim05);
## hydrological years from October to September, labeled by the
## calendar year in which they end
data(PTclim05, envir = environment())
ai <- subset(PTclim05, plot == 'P44005') |>
    moveYr() |>
    wlai()
subset(ai, year == 2001)
## the diagram of October 2000 to September 2001
plot(ai, year = 2001)
## the same block, with gray fills
plot(ai, hydro_year_id = 2000, hatch = FALSE)
}
