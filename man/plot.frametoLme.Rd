\name{plot.frametoLme}
\alias{plot.frametoLme}
\title{Plot frametoLme objects}
\description{Trellis plots of modeled fluctuations in \code{\link{frametoLme}} objects are obtained.}
\usage{\method{plot}{frametoLme}(x, ...)}
\arguments{
  \item{x}{An object inheriting from class \code{\link{frametoLme}}.}
  \item{\dots}{Further arguments passed to
\code{\link[nlme]{plot.nfnGroupedData}},
\code{\link[nlme]{plot.nmGroupedData}} or to trellis plotting
methods.}
}
\details{This method makes the fluctuation plot the
default visual output of a
\code{\link{frametoLme}} object, so users can call
\code{plot()} on the fit directly instead of
plotting its \code{$fluc} component. The method
delegates the display to the \pkg{nlme} plot
methods for grouped data
(\code{\link[nlme]{plot.nfnGroupedData}} and
\code{\link[nlme]{plot.nmGroupedData}}) whenever a
grouped fluctuation series is available. Before
plotting, it rebuilds a temporary plotting copy in
which numeric-like grouping variables are converted to
ordered factors with sorted levels, which avoids the
common packet order problem where groups such as
\code{3, 1, 2, 5, 6, 7, 4} are displayed in order of
appearance rather than natural numeric order.

If \code{x$fluc} is not available, the method tries to
reconstruct a residual fluctuation series from the
stored \code{lme} model and plots Pearson residuals
against the recorded time variable. The method also
recognizes helper arguments supplied through
\code{...}: \code{levels} to choose which grouping
variables define the conditioning structure,
\code{print.trellis} to print or suppress the trellis
object, and any regular trellis or
arguments of the \pkg{nlme} methods. By default,
\code{as.table = TRUE} is used when the plot is
conditioned on groups, which makes the panel order
follow a natural table layout.}
\value{A trellis object is returned invisibly and printed by default.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03);
## Zeide model with ln(time) and a tree-level random part, as in the
## frametoLme example
data(Prings05, envir = environment())
data(Pradii03, envir = environment())
fit <- Prings05 |>
    ringApply(lv = 2, fn = 'rtimes') |>
    ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
              fn = 'scacum') |>
    frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
               random = pdDiag(~ log(csx)))
## Pearson residuals of each tree over time
plot(fit)
## the trellis object can be kept instead of printed
tr <- plot(fit, print.trellis = FALSE)
class(tr)
}
