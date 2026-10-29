\name{update.frametoLme}
\alias{update.frametoLme}
\title{Update frametoLme objects}
\description{A \code{\link{frametoLme}} fit is refitted after one or more
arguments of its original call are changed.}
\usage{\method{update}{frametoLme}(object, 
    ..., evaluate = TRUE)}
\arguments{
  \item{object}{A \code{\link{frametoLme}} fit.}
  \item{\dots}{Named arguments replacing or adding entries in the
original \code{frametoLme()} call, such as
\code{method = 'ML'} or a new formula string.}
  \item{evaluate}{\code{logical}. Refit the model. If
\code{FALSE}, the rebuilt call is returned instead,
with the stored model data written as \code{..rd..}.}
}
\details{The original \code{frametoLme()} call
is rebuilt with the new arguments and evaluated
on the data stored with the fitted model
(\code{$model$data}), not on the data named in
the call. Refitting therefore works when the
original call came from a pipeline, and the
grouping columns, metadata and time columns
created upstream remain available. Arguments not
given keep their original values. Any argument of
\code{frametoLme()} can be changed, including
those it passes to \code{\link{lmeForm}},
\code{\link{tdForm}} or \code{\link[nlme]{lme}},
such as \code{method}, \code{random} or
\code{lev.rm}. The refitted model keeps the
data expression of the original call, so it
prints like the original. Fits compared with
\code{\link{anova.frametoLme}} must use the same
data, and models that differ in their fixed
effects must be fitted by maximum likelihood.}
\value{A refitted \code{\link{frametoLme}} fit, or the rebuilt call.}
\references{Pinheiro J. C., D. M. Bates. 2000.
Mixed-effects models in S and S-PLUS. Springer,
New York.}
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
## the same model fitted by maximum likelihood
fit_lnt <- update(fit, method = 'ML')
fixef(fit_lnt$model)
## the other Zeide time form, f(t) = t, also by maximum likelihood
fit_t <- update(fit_lnt, form = 'log(x) ~ log(csx) + time | tree')
c(ln_t = AIC(fit_lnt$model), t = AIC(fit_t$model))
}
