\name{frametoLme}
\alias{frametoLme}
\title{LME modeling}
\description{LME models are fitted to detrend multilevel ecological data series.}
\usage{frametoLme(rd, form = "lmeForm", 
    res.data = TRUE, 
    validate = c("none", 
        "warn", "stop"), 
    ...)}
\arguments{
  \item{rd}{\code{data.frame}. Multilevel ecological data series.}
  \item{form}{\code{character}. Any of two lme formulas:
\code{"lmeForm"} and \code{"tdForm"}. A formula
string containing \code{~} is used directly. If
\code{NULL}, the input data are returned unchanged.}
  \item{res.data}{\code{logical}. Save residuals as a multilevel
ecological data series. If \code{TRUE} then a data
frame of name \code{"fluc"} is added to output. Its
residuals are matched to the model-data rows by row
label, so each residual keeps its own time and
grouping values.}
  \item{validate}{\code{character}. Optional
validation mode.}
  \item{\dots}{Further arguments passed to \code{\link[nlme]{lme}} or to the
formula helper named in \code{form}.}
}
\details{This function implements
\code{\link[nlme]{lme}} to fit linear mixed-effects
models on multilevel ecological data series. When
\code{form} is a character without a tilde it is
resolved through the named formula helper, typically
\code{\link{lmeForm}} or \code{\link{tdForm}}; when
it already contains a tilde it is used directly as a
formula. When \code{form = NULL}, the function now
returns the processed input data unchanged, which makes
direct \code{ringApply()} pipelines safe. Before model
fitting, the function resolves the final formula and
keeps only the variables used by that formula and the
grouping columns. Rows with a missing value in any of
them are dropped from the fit without a message (use
\code{validate = "warn"} to be told how many). For
example, after \code{ringApply(Prings05, lv = 2, fn =
"rtimes")} the 16 rows where \code{\link{rtimes}} gives
no time are not in a fit that uses \code{time}, so the
model has 992 of the 1008 rows. Missing values in unused
columns, such as an alternative time index, do not
drop rows. The returned object receives
class \code{"frametoLme"}, which enables dedicated
\code{print()} and \code{summary()} methods.

Unless a random part is passed through \code{...},
it is \code{pdDiag} of the whole right-hand side of the
fixed formula: every fixed term, including covariates
written into a formula string, gets an uncorrelated
random effect at each grouping level. When a level has
very few groups, the default random part can give a
singular fit (variance components near zero) with
inflated standard errors of the fixed effects; a simpler
random part can then be given through \code{...}. In
the example, the bundled data (2 plots x 2 trees x 2
cores) support a tree intercept and a tree-level
\code{log(csx)} slope only.

With \code{form = "tdForm"}, the time term is linear by
default or logarithmic with \code{log.t = TRUE} (passed
through \code{...}); both are Zeide forms with the same
number of parameters, and the example compares them by
AIC under \code{method = "ML"}. Within cores,
\code{log(csx)} and the time term are strongly
correlated (r about 0.95 for time and 0.99 for ln(time)
on the bundled data), so their separate standard errors
depend on the random part.

Whole-number columns such as \code{year} are
treated as time, not as groups (see
\code{\link{cClass}}). To use year as a random
grouping level of a data frame \code{d}, either
declare it, as in \code{defineDataRoles(d, time =
"time", groups = c("year", "plot", "tree",
"sample"))}, or add a factor copy, as in
\code{d$fyear <- factor(d$year)}, and give
\code{frametoLme()} a formula string whose
grouping part names it, such as
\code{"log(x) ~ log(csx) + time | year"}. A factor
copy becomes a grouping column in the default
formulas, so name the grouping levels explicitly
or remove the copy with \code{lev.rm}.}
\value{\code{list} of class \code{"frametoLme"} with at least
components \code{model} and \code{call}, and optionally
\code{fluc}: Pearson residuals at every grouping level, each
matched by row label to its row of the model data and returned
with that row's time and grouping columns. If a correlation or
variance structure is passed to \code{lme} through \code{...},
these remain Pearson residuals, not normalized residuals.}
\references{Pinheiro J. C., D. M. Bates. 2000.
Mixed-effects models in S and S-PLUS. Springer,
New York.}
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
## the bundled data are a small demonstration set (2 plots x 2 trees x
## 2 cores) that supports variation among trees only: a tree intercept and
## a tree-level log(csx) slope (tree labels are unique across plots here,
## otherwise nest them, | plot/tree)
## both time forms are Zeide forms, f(t) = t (log.t = FALSE, the default)
## and f(t) = ln t (log.t = TRUE); fit both with ML and let the fit choose
fit_t <- dfm2 |>
    frametoLme(form = 'log(x) ~ log(csx) + time | tree',
               random = pdDiag(~ log(csx)), method = 'ML')
fit_lnt <- dfm2 |>
    frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
               random = pdDiag(~ log(csx)), method = 'ML')
anova(fit_t, fit_lnt)
## here the data prefer f(t) = ln t (lower AIC and BIC); refit it with REML
fit <- dfm2 |>
    frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
               random = pdDiag(~ log(csx)))
head(fit$fluc)
print(summary(fit))
}
