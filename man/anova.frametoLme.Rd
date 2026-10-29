\name{anova.frametoLme}
\alias{anova.frametoLme}
\title{Compare frametoLme objects}
\description{Analysis-of-variance tables of the fixed effects of one
\code{\link{frametoLme}} fit, or likelihood comparison of
several, computed by \code{\link[nlme]{anova.lme}} on the
underlying \code{lme} fits.}
\usage{\method{anova}{frametoLme}(object, 
    ..., test = TRUE, 
    type = c("sequential", 
        "marginal"), 
    adjustSigma = TRUE, 
    Terms, L, verbose = FALSE)}
\arguments{
  \item{object}{A \code{\link{frametoLme}} fit, or an \code{lme}
fit.}
  \item{\dots}{Further \code{frametoLme} or \code{lme} objects to
compare.}
  \item{test}{\code{logical}. With several models, compute
likelihood-ratio tests. Passed to
\code{\link[nlme]{anova.lme}}.}
  \item{type}{\code{character}. With
one model, sequential or
marginal F tests. See
\code{\link[nlme]{anova.lme}}.}
  \item{adjustSigma}{\code{logical}. With one model fitted
by ML, adjust the residual standard error. See
\code{\link[nlme]{anova.lme}}.}
  \item{Terms}{With one model, the fixed-effects terms tested
jointly, by name or position. See
\code{\link[nlme]{anova.lme}}.}
  \item{L}{With one model, a contrast matrix of the fixed effects. See
\code{\link[nlme]{anova.lme}}.}
  \item{verbose}{\code{logical}. Print the model calls. Passed
to \code{\link[nlme]{anova.lme}}.}
}
\details{With one model, the call is
forwarded to \code{\link[nlme]{anova.lme}} on
the stored \code{lme} fit, so sequential or
marginal F tests, tests of chosen terms and
contrast matrices work as in \pkg{nlme}. With
several models, their \code{lme} fits are
compared by AIC, BIC and log-likelihood, with
likelihood-ratio tests when the models differ in
their number of parameters; rows are named after
the objects given. \code{frametoLme} and
\code{lme} objects can be mixed. As in
\pkg{nlme}, models that differ in their fixed
effects are compared only when fitted by maximum
likelihood (\code{method = 'ML'}) to the same
data; \code{\link{update.frametoLme}} refits a
model with other arguments.}
\value{The \code{anova.lme} table of \code{\link[nlme]{anova.lme}}.}
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
## sequential and marginal F tests of the fixed effects
anova(fit)
anova(fit, type = 'marginal')
## F test of one term
anova(fit, Terms = 'log(time)')
## the two Zeide time forms, f(t) = ln t and f(t) = t, refitted by
## maximum likelihood and compared by AIC and BIC
fit_lnt <- update(fit, method = 'ML')
fit_t <- update(fit_lnt, form = 'log(x) ~ log(csx) + time | tree')
anova(fit_t, fit_lnt)
}
