update.frametoLme <- structure(function # Update frametoLme objects
### A \code{\link{frametoLme}} fit is refitted after one or more
### arguments of its original call are changed.
                        ##details<<The original \code{frametoLme()} call
                        ##is rebuilt with the new arguments and evaluated
                        ##on the data stored with the fitted model
                        ##(\code{$model$data}), not on the data named in
                        ##the call. Refitting therefore works when the
                        ##original call came from a pipeline, and the
                        ##grouping columns, metadata and time columns
                        ##created upstream remain available. Arguments not
                        ##given keep their original values. Any argument of
                        ##\code{frametoLme()} can be changed, including
                        ##those it passes to \code{\link{lmeForm}},
                        ##\code{\link{tdForm}} or \code{\link[nlme]{lme}},
                        ##such as \code{method}, \code{random} or
                        ##\code{lev.rm}. The refitted model keeps the
                        ##data expression of the original call, so it
                        ##prints like the original. Fits compared with
                        ##\code{\link{anova.frametoLme}} must use the same
                        ##data, and models that differ in their fixed
                        ##effects must be fitted by maximum likelihood.
                        ##references<<Pinheiro J. C., D. M. Bates. 2000.
                        ##Mixed-effects models in S and S-PLUS. Springer,
                        ##New York.
(
    object, ##<<A \code{\link{frametoLme}} fit.
    ..., ##<<Named arguments replacing or adding entries in the
         ##original \code{frametoLme()} call, such as
         ##\code{method = 'ML'} or a new formula string.
    evaluate = TRUE ##<<\code{logical}. Refit the model. If
                    ##\code{FALSE}, the rebuilt call is returned instead,
                    ##with the stored model data written as \code{..rd..}.
) {

    if(is.null(object$model) || is.null(object$model$data)) {
        stop("The frametoLme object does not contain stored model data.",
             call. = FALSE)
    }

    old_call <- object$call

    if(is.null(old_call) || !is.call(old_call)) {
        old_call <- quote(frametoLme(rd = ..rd..))
    }

    cl <- as.list(old_call)

    if(!length(cl) || !identical(as.character(cl[[1L]]), "frametoLme")) {
        cl[[1L]] <- as.name("frametoLme")
    }

    ## the data expression of the original call, put back in the call of
    ## the refitted object so that it prints like the original
    rd_expr <- NULL
    if("rd" %in% names(cl)) {
        rd_expr <- cl[["rd"]]
        cl[["rd"]] <- quote(..rd..)
    } else if(length(cl) >= 2L) {
        rd_expr <- cl[[2L]]
        cl[[2L]] <- quote(..rd..)
    } else {
        cl[["rd"]] <- quote(..rd..)
    }

    extras <- list(...)

    if(length(extras)) {
        nms <- names(extras)
        if(is.null(nms) || any(!nzchar(nms))) {
            stop("Unnamed update arguments are not supported for frametoLme.",
                 call. = FALSE)
        }
        for(nm in nms) {
            cl[[nm]] <- extras[[nm]]
        }
    }

    new_call <- as.call(cl)

    if(!isTRUE(evaluate)) {
        return(new_call)
    }

    rd <- object$model$data

    out <- eval(new_call,
                envir = list(..rd.. = rd),
                enclos = parent.frame())

    if(!is.null(rd_expr) && is.call(out$call)) {
        oc <- as.list(out$call)
        pos <- if("rd" %in% names(oc)) "rd" else 2L
        oc[pos] <- list(rd_expr)
        out$call <- as.call(oc)
    }

    out
### A refitted \code{\link{frametoLme}} fit, or the rebuilt call.
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
    ## the same model fitted by maximum likelihood
    fit_lnt <- update(fit, method = 'ML')
    fixef(fit_lnt$model)
    ## the other Zeide time form, f(t) = t, also by maximum likelihood
    fit_t <- update(fit_lnt, form = 'log(x) ~ log(csx) + time | tree')
    c(ln_t = AIC(fit_lnt$model), t = AIC(fit_t$model))
})
