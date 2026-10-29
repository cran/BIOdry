\name{plot.frametoFriedman}
\alias{plot.frametoFriedman}
\title{Plot Friedman fluctuation series over time}
\description{Trellis plots of detrended Friedman fluctuation series are obtained.}
\usage{\method{plot}{frametoFriedman}(x, 
    ..., levels = NULL, 
    print.trellis = TRUE)}
\arguments{
  \item{x}{Object inheriting from class \code{\link{frametoFriedman}}.}
  \item{\dots}{Further trellis arguments passed to the plotting method.}
  \item{levels}{Optional subset of grouping variables used for
conditioning. May be numeric positions or
character names.}
  \item{print.trellis}{\code{logical}. Print the trellis object.}
}
\details{The default display for
\code{\link{frametoFriedman}} objects is a
time-oriented trellis plot of modeled
fluctuations or residual-like detrended values over
the resolved time variable. This mimics the
time-panel displays commonly used with
\code{nlme} residual objects: the x-axis is the
time variable, the y-axis is the detrended
fluctuation column, and panels are defined by the
declared grouping hierarchy.

When \code{x$fluc} inherits from
\code{\link[nlme]{groupedData}}, its stored
formula is honored if it already has the form
\code{fluctuation ~ time | groups}. If that formula
cannot be used safely, the method reconstructs a
temporary grouped object with the fluctuation on the
left-hand side, the time variable on the x-axis, and
the selected grouping variables in the conditioning
structure.

Numeric-like grouping variables are normalized to
natural numeric order in the plotting copy only, so
panel strips such as zones appear in the order
\code{1, 2, 3, ...} instead of first appearance.}
\value{A trellis plot object. The object is printed by default.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
## relative time within trees, then cumulative growth scaled to the radii
data(Prings05, envir = environment())
data(Pradii03, envir = environment())
dfm2 <- Prings05 |>
    ringApply(lv = 2, fn = 'rtimes') |>
    ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
              fn = 'scacum')
ff <- frametoFriedman(dfm2)
## ratio index of each core over time
plot(ff)
## panels by plot, one line per core
plot(ff, levels = 'plot', layout = c(2, 1))
}
