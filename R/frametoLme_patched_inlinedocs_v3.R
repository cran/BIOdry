frametoLme <- structure(function # LME modeling
### LME models are fitted to detrend multilevel ecological data series.
                        ##details<<This function implements
                        ##\code{\link[nlme]{lme}} to fit linear mixed-effects
                        ##models on multilevel ecological data series. When
                        ##\code{form} is a character without a tilde it is
                        ##resolved through the named formula helper, typically
                        ##\code{\link{lmeForm}} or \code{\link{tdForm}}; when
                        ##it already contains a tilde it is used directly as a
                        ##formula. When \code{form = NULL}, the function now
                        ##returns the processed input data unchanged, which makes
                        ##direct \code{ringApply()} pipelines safe. Before model
                        ##fitting, the function resolves the final formula and
                        ##keeps only the variables used by that formula and the
                        ##grouping columns. Rows with a missing value in any of
                        ##them are dropped from the fit without a message (use
                        ##\code{validate = "warn"} to be told how many). For
                        ##example, after \code{ringApply(Prings05, lv = 2, fn =
                        ##"rtimes")} the 16 rows where \code{\link{rtimes}} gives
                        ##no time are not in a fit that uses \code{time}, so the
                        ##model has 992 of the 1008 rows. Missing values in unused
                        ##columns, such as an alternative time index, do not
                        ##drop rows. The returned object receives
                        ##class \code{"frametoLme"}, which enables dedicated
                        ##\code{print()} and \code{summary()} methods.
                        ##
                        ##Unless a random part is passed through \code{...},
                        ##it is \code{pdDiag} of the whole right-hand side of the
                        ##fixed formula: every fixed term, including covariates
                        ##written into a formula string, gets an uncorrelated
                        ##random effect at each grouping level. When a level has
                        ##very few groups, the default random part can give a
                        ##singular fit (variance components near zero) with
                        ##inflated standard errors of the fixed effects; a simpler
                        ##random part can then be given through \code{...}. In
                        ##the example, the bundled data (2 plots x 2 trees x 2
                        ##cores) support a tree intercept and a tree-level
                        ##\code{log(csx)} slope only.
                        ##
                        ##With \code{form = "tdForm"}, the time term is linear by
                        ##default or logarithmic with \code{log.t = TRUE} (passed
                        ##through \code{...}); both are Zeide forms with the same
                        ##number of parameters, and the example compares them by
                        ##AIC under \code{method = "ML"}. Within cores,
                        ##\code{log(csx)} and the time term are strongly
                        ##correlated (r about 0.95 for time and 0.99 for ln(time)
                        ##on the bundled data), so their separate standard errors
                        ##depend on the random part.
                        ##
                        ##Whole-number columns such as \code{year} are
                        ##treated as time, not as groups (see
                        ##\code{\link{cClass}}). To use year as a random
                        ##grouping level of a data frame \code{d}, either
                        ##declare it, as in \code{defineDataRoles(d, time =
                        ##"time", groups = c("year", "plot", "tree",
                        ##"sample"))}, or add a factor copy, as in
                        ##\code{d$fyear <- factor(d$year)}, and give
                        ##\code{frametoLme()} a formula string whose
                        ##grouping part names it, such as
                        ##\code{"log(x) ~ log(csx) + time | year"}. A factor
                        ##copy becomes a grouping column in the default
                        ##formulas, so name the grouping levels explicitly
                        ##or remove the copy with \code{lev.rm}.
                        ##references<<Pinheiro J. C., D. M. Bates. 2000.
                        ##Mixed-effects models in S and S-PLUS. Springer,
                        ##New York.
(
    rd, ##<<\code{data.frame}. Multilevel ecological data series.
    form = 'lmeForm', ##<<\code{character}. Any of two lme formulas:
                      ##\code{"lmeForm"} and \code{"tdForm"}. A formula
                      ##string containing \code{~} is used directly. If
                      ##\code{NULL}, the input data are returned unchanged.
    res.data = TRUE, ##<<\code{logical}. Save residuals as a multilevel
                     ##ecological data series. If \code{TRUE} then a data
                     ##frame of name \code{"fluc"} is added to output. Its
                     ##residuals are matched to the model-data rows by row
                     ##label, so each residual keeps its own time and
                     ##grouping values.
    validate = c('none','warn','stop'), ##<<\code{character}. Optional
                                        ##validation mode.
    ... ##<<Further arguments passed to \code{\link[nlme]{lme}} or to the
        ##formula helper named in \code{form}.
) {
    validate <- match.arg(validate)

    .signal <- function(msg) {
        if (validate == 'stop') stop(msg, call. = FALSE)
        if (validate == 'warn') warning(msg, call. = FALSE)
        invisible(NULL)
    }

    ## roles from column types (see cClass): whole-number columns are time
    ## candidates; factor and character columns that are not months are groups
    .guess_time_vars <- function(dat) {
        setdiff(.BIOdry_time_cols(dat), '.orig_row_id__')
    }

    .guess_group_vars <- function(dat, time_vars = character(0)) {
        setdiff(.BIOdry_group_cols(dat), time_vars)
    }

    .order_data <- function(dat) {
        tv <- unique(c(attr(dat, 'time_cols'), .guess_time_vars(dat)))
        gv <- unique(c(attr(dat, 'split_vars'), attr(dat, 'f.nm'),
                       .guess_group_vars(dat, tv)))
        gv <- gv[gv %in% names(dat)]
        tv <- tv[tv %in% names(dat)]
        ord_vars <- c(rev(gv), tv)
        ord_vars <- ord_vars[ord_vars %in% names(dat)]
        if (!length(ord_vars)) {
            return(list(data = dat, time_vars = tv, group_vars = gv))
        }
        ord_df <- lapply(ord_vars, function(nm) {
            x <- dat[[nm]]
            if (is.factor(x)) as.character(x) else x
        })
        o <- do.call(order, c(ord_df, list(na.last = TRUE, method = 'radix')))
        list(data = dat[o, , drop = FALSE], time_vars = tv, group_vars = gv)
    }

    .pr_cov <- function(form) {
        chf <- Reduce(paste, deparse(form))
        fnc <- gsub('.*~|\\|.*', '', chf)
        fnc <- paste('~', fnc, sep = '')
        formula(fnc)
    }

    .groups_from_formula <- function(form) {
        chf <- Reduce(paste, deparse(form))
        gp <- gsub('.*\\|', '', chf)
        gp <- trimws(gp)
        if (!nzchar(gp) || identical(gp, chf)) return(character(0))
        trimws(unlist(strsplit(gp, '/', fixed = TRUE)))
    }

    .build_grouped_formula <- function(resp, time_var, groups) {
        rhs <- time_var
        if (length(groups)) rhs <- paste(rhs, '|', paste(groups, collapse = '/'))
        formula(paste(resp, '~', rhs))
    }

    .resid_frame <- function(r.model, time_vars = character(0),
                             group_vars = character(0), fit_cols = NULL) {
        md <- r.model[['data']]
        time_vars <- unique(c(time_vars, attr(md, 'time_cols'),
                              .guess_time_vars(md)))
        group_vars <- unique(c(group_vars, attr(md, 'split_vars'),
                               attr(md, 'f.nm'),
                               .guess_group_vars(md, time_vars)))
        time_vars <- time_vars[time_vars %in% names(md)]
        group_vars <- group_vars[group_vars %in% names(md)]
        ## $fluc carries only the fitted columns; the model data also keep
        ## the other time columns for refitting
        if (!is.null(fit_cols)) {
            time_vars <- time_vars[time_vars %in% fit_cols]
            group_vars <- group_vars[group_vars %in% fit_cols]
        }
        lg <- tryCatch(ncol(data.frame(getGroups(md))), error = function(e) 1L)
        ## with one grouping level nlme names the residuals by group, not by
        ## model-data row, so they cannot be matched to rows; adding level 0
        ## makes nlme return a matrix labelled by model-data row
        dcum <- residuals(r.model, level = c(lg:1, 0L), type = 'p')
        dcum <- as.data.frame(dcum)
        dcum <- dcum[, setdiff(names(dcum), 'fixed'), drop = FALSE]
        nam. <- names(dcum)
        names(dcum) <- paste(nam., '.res', sep = '')
        idx <- match(rownames(dcum), rownames(md))
        if (anyNA(idx) || anyDuplicated(idx)) {
            stop('Residual rows could not be matched to model data rows.',
                 call. = FALSE)
        }
        ## a variable can be both a time column and a grouping level (e.g.
        ## year declared in groups); keep it once
        keep <- unique(c(time_vars, group_vars))
        if ('.orig_row_id__' %in% names(md)) keep <- c(keep, '.orig_row_id__')
        cbind(dcum, md[idx, keep, drop = FALSE])
    }

    if (!is.data.frame(rd)) stop("'rd' must be a data.frame.", call. = FALSE)
    if (!is.logical(res.data) || length(res.data) != 1L || is.na(res.data)) {
        stop("'res.data' must be TRUE or FALSE.", call. = FALSE)
    }

    ord <- .order_data(rd)
    rd_ord <- ord$data
    time_vars <- ord$time_vars
    group_vars <- ord$group_vars
    rd_ord[['.orig_row_id__']] <- seq_len(nrow(rd_ord))

    if (is.null(form)) {
        rd_ret <- rd_ord
        rd_ret[['.orig_row_id__']] <- NULL
        return(rd_ret)
    }

    if (is.character(form) && length(form) == 1L && grepl('~', form)) {
        formu <- formula(form)
    } else if (is.character(form) && length(form) == 1L) {
        arf <- arguSelect(rd_ord, fun = form, validate = validate, ...)
        formu <- do.call(form, arf)
    } else {
        formu <- formula(form)
    }

    prc <- .pr_cov(formu)
    environment(prc) <- .GlobalEnv

    group_vars_formula <- .groups_from_formula(formu)
    group_vars_formula <- group_vars_formula[group_vars_formula %in% names(rd_ord)]
    if (length(group_vars_formula)) group_vars <- unique(group_vars_formula)

    fit_vars <- unique(c(all.vars(formu), group_vars))
    ## keep the input's column order, so that grouping columns stay in the
    ## order the formula helpers read (outer level first = reverse order)
    fit_vars <- names(rd_ord)[names(rd_ord) %in% fit_vars]
    if (!length(fit_vars)) {
        stop('No valid model variables were found in the input data.', call. = FALSE)
    }

    fit_ok <- stats::complete.cases(rd_ord[, fit_vars, drop = FALSE])
    ## the fitting copy also keeps the time columns (e.g. time and year), so
    ## that fit$model$data can be refitted on either; rows are dropped only
    ## for missing fitted variables and grouping columns
    copy_vars <- names(rd_ord)[names(rd_ord) %in% c(fit_vars, time_vars)]
    rd_fit <- rd_ord[fit_ok, c(copy_vars, '.orig_row_id__'), drop = FALSE]

    if (!nrow(rd_fit)) {
        stop('No complete cases available for model fitting.', call. = FALSE)
    }
    if (nrow(rd_fit) < nrow(rd_ord)) {
        .signal(sprintf('Dropped %d rows with missing values in fitted variables before fitting.',
                        nrow(rd_ord) - nrow(rd_fit)))
    }

    if (!length(group_vars)) {
        stop('The resolved formula must include a grouping structure after "|".',
             call. = FALSE)
    }

    gd <- groupedData(formula = formu, data = rd_fit)
    ## the fitted time variable first, then the other time columns
    attr(gd, 'time_cols') <- c(intersect(time_vars, all.vars(formu)),
                               setdiff(time_vars, all.vars(formu)))
    attr(gd, 'split_vars') <- group_vars
    if (!is.null(attr(rd, 'BIOdry_meta'))) attr(gd, 'BIOdry_meta') <- attr(rd, 'BIOdry_meta')

    arl <- arguSelect(NULL, fun = 'lme', validate = validate, ...)
    arl[['fixed']] <- gd
    if (!'random' %in% names(arl)) arl[['random']] <- pdDiag(prc)
    if (!'control' %in% names(arl)) {
        arl[['control']] <- nlme::lmeControl(msMaxIter = 200)
    }

    mem. <- do.call(nlme::lme, arl)
    mem <- list(model = mem., call = sys.call())
    class(mem) <- c('frametoLme', class(mem))

    if (res.data) {
        residu <- .resid_frame(mem., time_vars = time_vars,
                               group_vars = group_vars, fit_cols = fit_vars)
        if ('.orig_row_id__' %in% names(residu)) {
            residu <- residu[order(residu[['.orig_row_id__']]), , drop = FALSE]
            residu[['.orig_row_id__']] <- NULL
        }
        attr(residu, 'time_cols') <- time_vars
        attr(residu, 'split_vars') <- group_vars
        if (!is.null(attr(rd, 'BIOdry_meta'))) attr(residu, 'BIOdry_meta') <- attr(rd, 'BIOdry_meta')
        resp <- names(residu)[1L]
        fit_rhs <- all.vars(formula(mem.)[[3L]])
        fit_time <- fit_rhs[fit_rhs %in% time_vars & fit_rhs %in% names(residu)]
        if (length(resp) == 1L && length(fit_time)) {
            fl_form <- .build_grouped_formula(resp, fit_time[1L], group_vars)
            mem[['fluc']] <- groupedData(fl_form, data = residu)
        } else {
            mem[['fluc']] <- residu
        }
    }

    mem
### \code{list} of class \code{"frametoLme"} with at least
### components \code{model} and \code{call}, and optionally
### \code{fluc}: Pearson residuals at every grouping level, each
### matched by row label to its row of the model data and returned
### with that row's time and grouping columns. If a correlation or
### variance structure is passed to \code{lme} through \code{...},
### these remain Pearson residuals, not normalized residuals.
}, ex=function() {
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
})

.frametoLme_summary_core <- function(object, ...) {

    s <- summary(object$model, ...)
    vc <- tryCatch(nlme::VarCorr(object$model), error = function(e) NULL)
    nobs_ <- tryCatch(stats::nobs(object$model), error = function(e) NA_integer_)
    logLik_ <- tryCatch(as.numeric(logLik(object$model)), error = function(e) NA_real_)
    AIC_ <- tryCatch(AIC(object$model), error = function(e) NA_real_)
    BIC_ <- tryCatch(BIC(object$model), error = function(e) NA_real_)

    out <- list(
        call = object$call,
        method = object$model$method,
        nobs = nobs_,
        logLik = logLik_,
        AIC = AIC_,
        BIC = BIC_,
        sigma = s$sigma,
        ## nlme's ngrps also counts the model matrix and response (X, y);
        ## the grouping levels are its first Q entries
        groups = object$model$dims$ngrps[seq_len(object$model$dims$Q)],
        fixed = s$tTable,
        corFixed = s$corFixed,
        varcorr = vc,
        has_fluc = !is.null(object$fluc)
    )
    out
}

summary.frametoLme <- structure(function # Summarize frametoLme objects
### A compact summary of a \code{\link{frametoLme}} object is obtained.
                        ##details<<This method extracts the main model
                        ##components that users typically inspect after
                        ##fitting a mixed-effects model: the original
                        ##package call, fitting method, sample size,
                        ##likelihood-based fit statistics, residual
                        ##standard deviation, grouping sizes, fixed-effect
                        ##table, and random-effect variance structure. It
                        ##avoids the very long Data line (a deparsed
                        ##\code{structure()} call) shown by \code{\link[nlme]{summary.lme}}
                        ##when the fitted \code{lme} object stores an
                        ##inlined \code{groupedData} call.
(
    object, ##<<Object inheriting from class \code{"frametoLme"}.
    ... ##<<Additional arguments passed to
        ##\code{\link[nlme]{summary.lme}}.
) {
    out <- .frametoLme_summary_core(object, ...)
    class(out) <- c('summary.frametoLme', class(out))
    out
### A compact summary object of class \code{"summary.frametoLme"}.
}, ex=function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    s <- dfm2 |>
        frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
                   random = pdDiag(~ log(csx))) |>
        summary()
    ## fit statistics and fixed effects
    s$AIC
    s$fixed
})

print.summary.frametoLme <- structure(function # Print compact frametoLme summaries
### A compact printed summary of a \code{\link{frametoLme}} object is obtained.
(
    x, ##<<Object inheriting from class \code{"summary.frametoLme"}.
    digits = max(3L, getOption('digits') - 2L), ##<<Integer number of digits.
    ... ##<<Unused extra arguments.
) {
    cat("Linear mixed-effects model fit\n\n")

    cat("Call:\n")
    print(x$call)

    cat("\nFit statistics:\n")
    cat("  method:", x$method, "\n")
    cat("  nobs  :", x$nobs, "\n")
    cat("  logLik:", format(signif(x$logLik, digits = digits)), "\n")
    cat("  AIC   :", format(signif(x$AIC, digits = digits)), "\n")
    cat("  BIC   :", format(signif(x$BIC, digits = digits)), "\n")
    cat("  sigma :", format(signif(x$sigma, digits = digits)), "\n")

    if (!is.null(x$groups)) {
        cat("\nGroups:\n")
        print(x$groups)
    }

    cat("\nFixed effects:\n")
    ## formatted column by column as in nlme's print.summary.lme, so that
    ## ordinary values and DF are not printed in scientific notation
    tt <- as.data.frame(x$fixed)
    pcol <- match("p-value", names(tt))
    for (nm in setdiff(names(tt), names(tt)[pcol])) {
        tt[[nm]] <- format(zapsmall(tt[[nm]]), digits = digits)
    }
    if (!is.na(pcol)) {
        pv <- round(x$fixed[, pcol], 4L)
        tt[[pcol]] <- ifelse(pv == 0, "<.0001", format(pv))
    }
    print(tt)

    if (!is.null(x$varcorr)) {
        cat("\nRandom effects:\n")
        print(x$varcorr)
    }

    if (isTRUE(x$has_fluc)) {
        cat("\nResidual series are available in $fluc.\n")
    }

    invisible(x)
### The printed compact summary.
}, ex=function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    dfm2 |>
        frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
                   random = pdDiag(~ log(csx))) |>
        summary() |>
        print(digits = 3)
})

print.frametoLme <- structure(function # Print frametoLme objects
### A compact printed representation of a \code{\link{frametoLme}} object is obtained.
                        ##details<<This method prints the compact summary
                        ##instead of the raw list returned by
                        ##\code{\link{frametoLme}}, so users do not see the
                        ##full nested list with model, call, and residual
                        ##series every time the object name is evaluated.
(
    x, ##<<Object inheriting from class \code{"frametoLme"}.
    ... ##<<Additional arguments passed to \code{\link{summary.frametoLme}}.
) {
    sx <- summary.frametoLme(x, ...)
    print.summary.frametoLme(sx, ...)
    invisible(x)
### The compact printed representation.
}, ex=function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    ## printing shows the compact summary, not the nested list
    dfm2 |>
        frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
                   random = pdDiag(~ log(csx))) |>
        print()
})

## summary.modelFrame <- structure(function # Summarize modelFrame objects
## ### A compact summary of a \code{\link{modelFrame}} object is obtained.
##                         ##details<<This method now returns the same compact
##                         ##summary structure used by
##                         ##\code{\link{summary.frametoLme}}. It therefore
##                         ##summarizes the fitted mixed-effects model without
##                         ##showing the very long embedded data call that can
##                         ##appear in \code{\link[nlme]{summary.lme}}.
## (
##     object, ##<<Object inheriting from class \code{\link{modelFrame}}.
##     ... ##<<Additional arguments passed to
##         ##\code{\link[nlme]{summary.lme}}.
## ) {
##     out <- .frametoLme_summary_core(object, ...)
##     class(out) <- c('summary.modelFrame', 'summary.frametoLme', class(out))
##     out
## ### A compact summary object for \code{\link{modelFrame}}.
## }, ex=function() {

##     ## TRW chronology (mm) and inside-bark radii
##     data(Pchron, envir = environment())

##     ## Parameters of allometric model to compute Diameter at Breast
##     ## Height over bark (DBH, cm) from diameter inside bark (dib, cm)
##     ## and Total Tree Biomass (TTB, kg tree -1) from DBH
##     biom_param <- c(2.87, 0.85, 0.05, 2.5)

##     ## /donttest{
##     ## trwf <- modelFrame(Pchron,
##     ##                    to = 'cm',
##     ##                    MoreArgs = list(mp = c(2,1, biom_param)),
##     ##                    log.t = FALSE,
##     ##                    on.time = FALSE)
##     ## summary(trwf)
##     ## }
## })

## print.summary.modelFrame <- structure(function # Print compact modelFrame summaries
## ### A compact printed summary of a \code{\link{modelFrame}} object is obtained.
## (
##     x, ##<<Object inheriting from class \code{"summary.modelFrame"}.
##     digits = max(3L, getOption('digits') - 2L), ##<<Integer number of digits.
##     ... ##<<Unused extra arguments.
## ) {
##     print.summary.frametoLme(x, digits = digits, ...)
##     invisible(x)
## ### The printed compact summary.
## }, ex=function() {

##     ## See the example in \code{\link{summary.modelFrame}}.
## })

## print.modelFrame <- structure(function # Print modelFrame objects
## ### A compact printed representation of a \code{\link{modelFrame}} object is obtained.
##                         ##details<<This method prints the compact summary of
##                         ##the fitted model instead of printing the full list
##                         ##stored in the \code{\link{modelFrame}} object.
## (
##     x, ##<<Object inheriting from class \code{\link{modelFrame}}.
##     ... ##<<Additional arguments passed to \code{\link{summary.modelFrame}}.
## ) {
##     sx <- summary.modelFrame(x, ...)
##     print.summary.modelFrame(sx, ...)
##     invisible(x)
## ### The compact printed representation.
## }, ex=function() {

##     ## See the example in \code{\link{summary.modelFrame}}.
## })

