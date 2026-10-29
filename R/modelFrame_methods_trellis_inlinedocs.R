.modelFrame_terminal_kind <- function(object) {
    term <- attr(object, 'terminal', exact = TRUE)
    if (is.character(term) && length(term) == 1L) return(term)
    if (inherits(object, 'frametoFriedman')) return('frametoFriedman')
    if (inherits(object, 'frametoLme')) return('frametoLme')
    if (is.list(object) && !is.null(object$model)) {
        if (inherits(object$model, 'lme')) return('frametoLme')
        if (is.list(object$model) && identical(object$model$method, 'Friedman supersmoother')) {
            return('frametoFriedman')
        }
    }
    'none'
}

.unwrap_modelFrame_for_anova <- function(object) {
    kind <- .modelFrame_terminal_kind(object)
    if (identical(kind, 'frametoLme')) {
        if (inherits(object, 'frametoLme')) return(object$model)
        if (is.list(object) && !is.null(object$model) && inherits(object$model, 'lme')) return(object$model)
    }
    if (identical(kind, 'frametoFriedman')) {
        if (is.list(object) && !is.null(object$post_lme)) {
            if (inherits(object$post_lme, 'frametoLme')) return(object$post_lme$model)
            if (inherits(object$post_lme, 'modelFrame')) return(.unwrap_modelFrame_for_anova(object$post_lme))
            if (inherits(object$post_lme, 'lme')) return(object$post_lme)
        }
        stop(paste(
            'anova() is not defined for a pure frametoFriedman result.',
            'Fit frametoLme() on the detrended output, or store a second-stage fit in $post_lme.'
        ), call. = FALSE)
    }
    stop("anova() is not defined for a 'modelFrame' object returned with form = NULL.", call. = FALSE)
}

summary.modelFrame <- structure(function # Summarize modelFrame objects
### A compact summary of a \code{\link{modelFrame}} object is obtained.
                        ##details<< This method wraps the terminal
                        ##fitter stored in the \code{\link{modelFrame}}
                        ##object. If \code{form = NULL}, the method
                        ##returns a compact description of the processed
                        ##long-format data and its grouped plotting
                        ##structure.
(
    object, ##<<Object inheriting from class \code{\link{modelFrame}}.
    ... ##<<Additional arguments passed to the terminal summary method.
) {
    kind <- .modelFrame_terminal_kind(object)
    if (identical(kind, 'frametoLme')) {
        out <- summary.frametoLme(object, ...)
        class(out) <- c('summary.modelFrame', 'summary.frametoLme', class(out))
        return(out)
    }
    if (identical(kind, 'frametoFriedman')) {
        out <- summary.frametoFriedman(object, ...)
        class(out) <- c('summary.modelFrame', 'summary.frametoFriedman', class(out))
        return(out)
    }
    fl <- attr(object, 'fluc', exact = TRUE)
    out <- list(
        terminal = 'none',
        nobs = NROW(object),
        nvars = NCOL(object),
        time = if (!is.null(fl)) attr(fl, 'mf_time', exact = TRUE) else character(0),
        response = if (!is.null(fl)) attr(fl, 'mf_response', exact = TRUE) else character(0),
        groups = if (!is.null(fl)) attr(fl, 'mf_groups', exact = TRUE) else character(0),
        columns = names(object),
        call = attr(object, 'call', exact = TRUE)
    )
    class(out) <- c('summary.modelFrame', 'summary.modelFrameData', 'list')
    out
### A compact summary object for \code{\link{modelFrame}}.
}, ex=function() {
    data(Pchron, envir = environment())
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    s <- Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   log.t = FALSE, on.time = FALSE) |>
        summary()
    ## fit statistics and fixed effects of the terminal model
    s$AIC
    s$fixed
    ## with form = NULL the summary describes the processed data
    Pchron |>
        modelFrame(form = NULL, to = 'cm',
                   MoreArgs = list(mp = c(2, 1, biom_param))) |>
        summary()
})

print.summary.modelFrame <- structure(function # Print compact modelFrame summaries
### A compact printed summary of a \code{\link{modelFrame}} object is obtained.
(
    x, ##<<Object inheriting from class \code{"summary.modelFrame"}.
    digits = max(3L, getOption('digits') - 2L), ##<<Integer number of digits.
    ... ##<<Unused extra arguments.
) {
    if (inherits(x, 'summary.frametoFriedman')) {
        print.summary.frametoFriedman(x, digits = digits, ...)
        return(invisible(x))
    }
    if (inherits(x, 'summary.frametoLme')) {
        print.summary.frametoLme(x, digits = digits, ...)
        return(invisible(x))
    }
    if (inherits(x, 'summary.modelFrameData')) {
        cat("Processed multilevel data returned by modelFrame(form = NULL)\n")
        if (!is.null(x$call)) {
            cat("\nCall:\n")
            print(x$call)
        }
        cat(sprintf("\nRows: %d\nColumns: %d\n", x$nobs, x$nvars))
        if (length(x$response)) cat("Response: ", paste(x$response, collapse = ', '), "\n", sep = "")
        if (length(x$time)) cat("Time: ", paste(x$time, collapse = ', '), "\n", sep = "")
        if (length(x$groups)) cat("Groups: ", paste(x$groups, collapse = ' / '), "\n", sep = "")
        invisible(x)
    } else {
        stop('print.summary.modelFrame() received an unknown summary structure.', call. = FALSE)
    }
### The printed compact summary.
}, ex=function() {
    data(Pchron, envir = environment())
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   log.t = FALSE, on.time = FALSE) |>
        summary() |>
        print(digits = 3)
})

print.modelFrame <- structure(function # Print modelFrame objects
### A compact printed representation of a \code{\link{modelFrame}} object is obtained.
(
    x, ##<<Object inheriting from class \code{\link{modelFrame}}.
    ... ##<<Additional arguments passed to \code{\link{summary.modelFrame}}.
) {
    sx <- summary.modelFrame(x, ...)
    print.summary.modelFrame(sx, ...)
    invisible(x)
### The compact printed representation.
}, ex=function() {
    data(Pchron, envir = environment())
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    ## printing shows a compact description, not the whole data
    Pchron |>
        modelFrame(form = NULL, to = 'cm',
                   MoreArgs = list(mp = c(2, 1, biom_param))) |>
        print()
})

anova.modelFrame <- structure(function # Compare modelFrame objects
### Models in \code{\link{modelFrame}} objects are compared with an
### ANOVA method when the terminal fit is based on
### \code{\link[nlme]{lme}}.
(
    object, ##<<An object inheriting from class \code{\link{modelFrame}}.
    ..., ##<<Other optional fitted model objects inheriting from classes
         ##\code{"modelFrame"}, \code{"lme"}, \code{"lm"}, among
         ##others accepted by \code{\link[nlme]{anova.lme}}.
    test,
    type,
    adjustSigma,
    Terms,
    L,
    verbose
) {
    extra <- list()
    if (!missing(test)) extra$test <- test
    if (!missing(type)) extra$type <- type
    if (!missing(adjustSigma)) extra$adjustSigma <- adjustSigma
    if (!missing(Terms)) extra$Terms <- Terms
    if (!missing(L)) extra$L <- L
    if (!missing(verbose)) extra$verbose <- verbose

    call_exprs <- as.list(substitute(list(object, ...)))[-1L]
    mods <- c(list(object), list(...))
    labs <- vapply(call_exprs, function(z) paste(deparse(z), collapse = ''), character(1))
    mods <- lapply(mods, .unwrap_modelFrame_for_anova)

    out <- do.call(anova, c(mods, extra))
    if (length(mods) > 1L && is.data.frame(out)) {
        nr <- min(nrow(out), length(labs))
        rownames(out)[seq_len(nr)] <- labs[seq_len(nr)]
    }
    out
### A data frame inheriting from class \code{"anova.lme"} when the
### underlying terminal fit is mixed-effects.
}, ex=function() {
    ## ring-width chronologies of eight cores and the allometric
    ## parameters of the modelFrame example
    data(Pchron, envir = environment())
    biom_param <- c(2.87, 0.85, 0.05, 2.5)
    ## the biomass model with the two Zeide time forms, f(t) = t and
    ## f(t) = ln t, fitted by maximum likelihood
    m_t <- Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   log.t = FALSE, method = 'ML')
    m_lnt <- Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   log.t = TRUE, method = 'ML')
    ## sequential F tests of the fixed effects of one model
    anova(m_t)
    ## both forms have the same number of parameters: compare AIC and BIC
    anova(m_t, m_lnt)
})
