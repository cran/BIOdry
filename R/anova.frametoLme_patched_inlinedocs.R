anova.frametoLme <- structure(function # Compare frametoLme objects
### Analysis-of-variance tables of the fixed effects of one
### \code{\link{frametoLme}} fit, or likelihood comparison of
### several, computed by \code{\link[nlme]{anova.lme}} on the
### underlying \code{lme} fits.
                        ##details<<With one model, the call is
                        ##forwarded to \code{\link[nlme]{anova.lme}} on
                        ##the stored \code{lme} fit, so sequential or
                        ##marginal F tests, tests of chosen terms and
                        ##contrast matrices work as in \pkg{nlme}. With
                        ##several models, their \code{lme} fits are
                        ##compared by AIC, BIC and log-likelihood, with
                        ##likelihood-ratio tests when the models differ in
                        ##their number of parameters; rows are named after
                        ##the objects given. \code{frametoLme} and
                        ##\code{lme} objects can be mixed. As in
                        ##\pkg{nlme}, models that differ in their fixed
                        ##effects are compared only when fitted by maximum
                        ##likelihood (\code{method = 'ML'}) to the same
                        ##data; \code{\link{update.frametoLme}} refits a
                        ##model with other arguments.
                        ##references<<Pinheiro J. C., D. M. Bates. 2000.
                        ##Mixed-effects models in S and S-PLUS. Springer,
                        ##New York.
(
    object, ##<<A \code{\link{frametoLme}} fit, or an \code{lme}
            ##fit.
    ..., ##<<Further \code{frametoLme} or \code{lme} objects to
         ##compare.
    test = TRUE, ##<<\code{logical}. With several models, compute
                 ##likelihood-ratio tests. Passed to
                 ##\code{\link[nlme]{anova.lme}}.
    type = c("sequential", "marginal"), ##<<\code{character}. With
                                        ##one model, sequential or
                                        ##marginal F tests. See
                                        ##\code{\link[nlme]{anova.lme}}.
    adjustSigma = TRUE, ##<<\code{logical}. With one model fitted
                        ##by ML, adjust the residual standard error. See
                        ##\code{\link[nlme]{anova.lme}}.
    Terms, ##<<With one model, the fixed-effects terms tested
           ##jointly, by name or position. See
           ##\code{\link[nlme]{anova.lme}}.
    L, ##<<With one model, a contrast matrix of the fixed effects. See
       ##\code{\link[nlme]{anova.lme}}.
    verbose = FALSE ##<<\code{logical}. Print the model calls. Passed
                    ##to \code{\link[nlme]{anova.lme}}.
) {
  dots_call <- match.call(expand.dots = FALSE)$...
  dots <- list(...)

  .unwrap_lme <- function(x) {
    if(inherits(x, "frametoLme")) {
      if(is.null(x$model) || !inherits(x$model, "lme")) {
        stop("Each frametoLme object must contain a valid lme fit in $model.",
             call. = FALSE)
      }
      return(x$model)
    }
    if(inherits(x, "lme")) {
      return(x)
    }
    stop("anova.frametoLme only supports frametoLme or lme objects.",
         call. = FALSE)
  }

  .label_from_expr <- function(expr, default = "") {
    if(is.null(expr)) return(default)
    txt <- paste(deparse(expr, width.cutoff = 500L), collapse = "")
    if(!nzchar(txt)) default else txt
  }

  first_model <- .unwrap_lme(object)

  ## Single-object path: forward full single-model interface to anova.lme
  if(length(dots) == 0L) {
    return(nlme::anova.lme(first_model,
                           test = test,
                           type = type,
                           adjustSigma = adjustSigma,
                           Terms = Terms,
                           L = L,
                           verbose = verbose))
  }

  other_models <- lapply(dots, .unwrap_lme)
  models <- c(list(first_model), other_models)

  ## Compare multiple models. Use only arguments relevant to multi-model
  ## anova.lme dispatch.
  out <- do.call(nlme::anova.lme,
                 c(models, list(test = test, verbose = verbose)))

  ## Try to preserve meaningful row names from the original call.
  first_label <- .label_from_expr(match.call()$object, default = "model1")
  other_labels <- vapply(dots_call, .label_from_expr,
                         character(1L),
                         default = "")
  labels <- c(first_label, other_labels)
  labels[!nzchar(labels)] <- paste0("model", seq_along(labels))[!nzchar(labels)]

  if(is.data.frame(out) && nrow(out) == length(labels)) {
    rownames(out) <- labels
  }

  out
### The \code{anova.lme} table of \code{\link[nlme]{anova.lme}}.
}, ex = function() {
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
})
