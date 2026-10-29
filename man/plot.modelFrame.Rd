\name{plot.modelFrame}
\alias{plot.modelFrame}
\title{Plot modelFrame objects}
\description{Trellis plots of fluctuations in \code{\link{modelFrame}} objects are obtained.}
\usage{\method{plot}{modelFrame}(x, ...)}
\arguments{
  \item{x}{An object inheriting from class \code{\link{modelFrame}}.}
  \item{\dots}{Further trellis arguments passed to the plotting method.}
}
\details{This method is a thin wrapper over
the \pkg{nlme} plot methods for grouped data
(\code{\link[nlme]{plot.nfnGroupedData}},
\code{\link[nlme]{plot.nmGroupedData}}) and
trellis graphics. For terminal
\code{\link{frametoLme}} fits, the default
display emulates \code{plot(model,
resid(., type = "p") ~ fitted(.) | GroupID)}
by plotting Pearson residuals against fitted
values in panels defined by the selected
grouping levels. For terminal
\code{\link{frametoFriedman}} fits, the
default display plots the detrended
fluctuation against the smooth fit. For
\code{form = NULL} outputs, the method plots
the processed fluctuation against time.

All regular trellis arguments are accepted
through \code{...}. The method recognizes
three additional helper arguments in
\code{...}: \code{levels} to choose which
grouping variables define the conditioning
panels, \code{reference} to control the
horizontal reference line, and
\code{print.trellis} to print or suppress the
trellis object.}
\value{A trellis plot object. The object is printed by default.}

\author{Wilson Lara-Henao [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>),
Felipe Bravo [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## ring-width chronologies of eight cores and the allometric
## parameters of the modelFrame example
data(Pchron, envir = environment())
biom_param <- c(2.87, 0.85, 0.05, 2.5)
trwf <- Pchron |>
    modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
               log.t = FALSE, on.time = FALSE)
## Pearson residuals against fitted values for each core
plot(trwf)
## panels by plot only
plot(trwf, levels = 'plot', layout = c(2, 1))
## with form = NULL, the processed series are plotted over time
Pchron |>
    modelFrame(form = NULL, to = 'cm',
               MoreArgs = list(mp = c(2, 1, biom_param))) |>
    plot()
}
