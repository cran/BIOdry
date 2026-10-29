plot.frametoLme <- structure(function # Plot frametoLme objects
### Trellis plots of modeled fluctuations in \code{\link{frametoLme}} objects are obtained.
                        ##details<<This method makes the fluctuation plot the
                        ##default visual output of a
                        ##\code{\link{frametoLme}} object, so users can call
                        ##\code{plot()} on the fit directly instead of
                        ##plotting its \code{$fluc} component. The method
                        ##delegates the display to the \pkg{nlme} plot
                        ##methods for grouped data
                        ##(\code{\link[nlme]{plot.nfnGroupedData}} and
                        ##\code{\link[nlme]{plot.nmGroupedData}}) whenever a
                        ##grouped fluctuation series is available. Before
                        ##plotting, it rebuilds a temporary plotting copy in
                        ##which numeric-like grouping variables are converted to
                        ##ordered factors with sorted levels, which avoids the
                        ##common packet order problem where groups such as
                        ##\code{3, 1, 2, 5, 6, 7, 4} are displayed in order of
                        ##appearance rather than natural numeric order.
                        ##
                        ##If \code{x$fluc} is not available, the method tries to
                        ##reconstruct a residual fluctuation series from the
                        ##stored \code{lme} model and plots Pearson residuals
                        ##against the recorded time variable. The method also
                        ##recognizes helper arguments supplied through
                        ##\code{...}: \code{levels} to choose which grouping
                        ##variables define the conditioning structure,
                        ##\code{print.trellis} to print or suppress the trellis
                        ##object, and any regular trellis or
                        ##arguments of the \pkg{nlme} methods. By default,
                        ##\code{as.table = TRUE} is used when the plot is
                        ##conditioned on groups, which makes the panel order
                        ##follow a natural table layout.
(
    x, ##<<An object inheriting from class \code{\link{frametoLme}}.
    ... ##<<Further arguments passed to
        ##\code{\link[nlme]{plot.nfnGroupedData}},
        ##\code{\link[nlme]{plot.nmGroupedData}} or to trellis plotting
        ##methods.
) {
    dots <- list(...)

    .pull_dot <- function(dots, name, default = NULL) {
        if (name %in% names(dots)) {
            val <- dots[[name]]
            dots[[name]] <- NULL
            return(list(value = val, dots = dots))
        }
        list(value = default, dots = dots)
    }

    tmp <- .pull_dot(dots, 'levels', default = NULL)
    panel_levels <- tmp$value
    dots <- tmp$dots

    tmp <- .pull_dot(dots, 'print.trellis', default = TRUE)
    print_trellis <- isTRUE(tmp$value)
    dots <- tmp$dots

    if (!'as.table' %in% names(dots)) dots$as.table <- TRUE
    ## fill the panels (nlme's default aspect = "xy" banks them to thin
    ## strips) and label the residual axis, unless the user gives these
    if (!'aspect' %in% names(dots)) dots$aspect <- 'fill'
    if (!'ylab' %in% names(dots)) dots$ylab <- 'Pearson residuals'

    .group_vars_from_formula <- function(form) {
        if (is.null(form)) return(character(0))
        if (length(form) == 2L) {
            ## one-sided grouping formula, e.g. ~ plot/tree/sample from
            ## nlme::getGroupsFormula()
            cond <- form[[2L]]
        } else {
            rhs <- form[[3L]]
            if (is.call(rhs) && identical(rhs[[1L]], as.name('|'))) {
                cond <- rhs[[3L]]
            } else return(character(0))
        }
        out <- character(0)
        walk <- function(expr) {
            if (is.name(expr)) {
                out <<- c(out, as.character(expr))
            } else if (is.call(expr) && identical(expr[[1L]], as.name('/'))) {
                walk(expr[[2L]])
                walk(expr[[3L]])
            } else {
                out <<- c(out, all.vars(expr))
            }
        }
        walk(cond)
        unique(out)
    }

    .time_from_formula <- function(form) {
        if (is.null(form)) return(character(0))
        rhs <- form[[3L]]
        if (is.call(rhs) && identical(rhs[[1L]], as.name('|'))) rhs <- rhs[[2L]]
        vars <- all.vars(rhs)
        if (length(vars)) vars[1L] else character(0)
    }

    .lhs_from_formula <- function(form) {
        if (is.null(form)) return(character(0))
        vars <- all.vars(form[[2L]])
        if (length(vars)) vars[1L] else character(0)
    }

    ## whole-number columns are time candidates (see cClass)
    .guess_time_vars <- function(dat) {
        .BIOdry_time_cols(dat)
    }

    .normalize_group <- function(z) {
        if (is.factor(z)) {
            lv <- levels(z)
            num <- suppressWarnings(as.numeric(as.character(lv)))
            if (!all(is.na(num)) && !anyNA(num[is.finite(num)])) {
                return(factor(as.character(z), levels = as.character(sort(unique(num[is.finite(num)])))))
            }
            return(factor(as.character(z), levels = sort(unique(as.character(z)))))
        }
        if (is.character(z)) {
            num <- suppressWarnings(as.numeric(z))
            if (!all(is.na(z)) && !anyNA(num[!is.na(z)])) {
                return(factor(z, levels = as.character(sort(unique(num[is.finite(num)])))))
            }
            return(factor(z, levels = sort(unique(z))))
        }
        if (is.logical(z)) return(factor(z, levels = c(FALSE, TRUE)))
        if (is.numeric(z) || is.integer(z)) {
            lev <- sort(unique(z[!is.na(z)]))
            return(factor(z, levels = lev))
        }
        z
    }

    .select_groups <- function(group_vars, levels_spec) {
        if (is.null(levels_spec) || !length(group_vars)) return(group_vars)
        if (is.numeric(levels_spec)) {
            idx <- as.integer(levels_spec)
            idx <- idx[is.finite(idx) & idx >= 1L & idx <= length(group_vars)]
            if (!length(idx)) return(group_vars)
            return(group_vars[idx])
        }
        if (is.character(levels_spec)) {
            keep <- intersect(as.character(levels_spec), group_vars)
            if (!length(keep)) return(group_vars)
            return(keep)
        }
        group_vars
    }

    .build_grouped_formula <- function(yvar, xvar, group_vars = character(0)) {
        if (!length(group_vars)) {
            as.formula(sprintf('%s ~ %s', yvar, xvar))
        } else {
            as.formula(sprintf('%s ~ %s | %s', yvar, xvar, paste(group_vars, collapse = '/')))
        }
    }

    .ensure_grouped_fluc <- function(fl, panel_levels = NULL) {
        dat <- as.data.frame(fl)
        form <- if (inherits(fl, 'groupedData')) formula(fl) else NULL

        response <- if (!is.null(form)) .lhs_from_formula(form) else names(dat)[1L]
        time_var <- if (!is.null(form)) .time_from_formula(form) else {
            tv <- attr(dat, 'time_cols', exact = TRUE)
            tv <- unique(c(tv, .guess_time_vars(dat)))
            tv[tv %in% names(dat)][1L]
        }
        if (is.na(time_var) || !nzchar(time_var)) time_var <- character(0)

        group_vars <- if (!is.null(form)) .group_vars_from_formula(form) else {
            gv <- attr(dat, 'split_vars', exact = TRUE)
            gv[gv %in% names(dat)]
        }
        group_vars <- .select_groups(group_vars, panel_levels)
        if (length(group_vars)) dat[group_vars] <- lapply(dat[group_vars], .normalize_group)

        if (length(time_var) && nzchar(time_var)) {
            nlme::groupedData(.build_grouped_formula(response, time_var, group_vars), data = dat)
        } else {
            dat
        }
    }

    .rebuild_from_model <- function(object, panel_levels = NULL) {
        mod <- object$model
        dat <- tryCatch(nlme::getData(mod), error = function(e) NULL)
        if (is.null(dat) && !is.null(mod$data)) dat <- mod$data
        if (is.null(dat)) stop("Could not recover fitted data from the stored 'lme' model.", call. = FALSE)
        dat <- as.data.frame(dat)

        fl_name <- '.model_resid__'
        dat[[fl_name]] <- as.numeric(resid(mod, type = 'p'))

        time_var <- attr(dat, 'time_cols', exact = TRUE)
        time_var <- unique(c(time_var, .guess_time_vars(dat)))
        time_var <- time_var[time_var %in% names(dat)]
        ## the time variable of the fitted fixed formula comes first: the
        ## model data can hold other time columns too
        fit_rhs <- tryCatch(all.vars(formula(mod)[[3L]]), error = function(e) character(0))
        time_var <- c(intersect(fit_rhs, time_var), setdiff(time_var, fit_rhs))
        time_var <- if (length(time_var)) time_var[1L] else character(0)
        if (!length(time_var)) stop("Could not identify a time variable for plotting residual fluctuations.", call. = FALSE)

        gf <- tryCatch(nlme::getGroupsFormula(mod), error = function(e) NULL)
        group_vars <- .group_vars_from_formula(gf)
        group_vars <- group_vars[group_vars %in% names(dat)]
        group_vars <- .select_groups(group_vars, panel_levels)
        if (length(group_vars)) dat[group_vars] <- lapply(dat[group_vars], .normalize_group)

        nlme::groupedData(.build_grouped_formula(fl_name, time_var, group_vars), data = dat)
    }

    fl <- if (!is.null(x$fluc)) x$fluc else NULL
    gfl <- if (!is.null(fl)) .ensure_grouped_fluc(fl, panel_levels = panel_levels) else .rebuild_from_model(x, panel_levels = panel_levels)

    tr <- if (inherits(gfl, 'groupedData')) {
        do.call(plot, c(list(x = gfl), dots))
    } else {
        yvar <- names(gfl)[1L]
        xvar <- attr(gfl, 'time_cols', exact = TRUE)
        xvar <- xvar[xvar %in% names(gfl)]
        if (!length(xvar)) xvar <- names(gfl)[2L]
        do.call(lattice::xyplot, c(list(stats::as.formula(sprintf('%s ~ %s', yvar, xvar)), data = gfl), dots))
    }

    if (print_trellis && inherits(tr, 'trellis')) print(tr)
    invisible(tr)
### A trellis object is returned invisibly and printed by default.
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
    ## Pearson residuals of each tree over time
    plot(fit)
    ## the trellis object can be kept instead of printed
    tr <- plot(fit, print.trellis = FALSE)
    class(tr)
})
