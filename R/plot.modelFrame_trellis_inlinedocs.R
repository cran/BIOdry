plot.modelFrame <- structure(function # Plot modelFrame objects
### Trellis plots of fluctuations in \code{\link{modelFrame}} objects are obtained.
                        ##details<< This method is a thin wrapper over
                        ##the \pkg{nlme} plot methods for grouped data
                        ##(\code{\link[nlme]{plot.nfnGroupedData}},
                        ##\code{\link[nlme]{plot.nmGroupedData}}) and
                        ##trellis graphics. For terminal
                        ##\code{\link{frametoLme}} fits, the default
                        ##display emulates \code{plot(model,
                        ##resid(., type = "p") ~ fitted(.) | GroupID)}
                        ##by plotting Pearson residuals against fitted
                        ##values in panels defined by the selected
                        ##grouping levels. For terminal
                        ##\code{\link{frametoFriedman}} fits, the
                        ##default display plots the detrended
                        ##fluctuation against the smooth fit. For
                        ##\code{form = NULL} outputs, the method plots
                        ##the processed fluctuation against time.
                        ##
                        ##All regular trellis arguments are accepted
                        ##through \code{...}. The method recognizes
                        ##three additional helper arguments in
                        ##\code{...}: \code{levels} to choose which
                        ##grouping variables define the conditioning
                        ##panels, \code{reference} to control the
                        ##horizontal reference line, and
                        ##\code{print.trellis} to print or suppress the
                        ##trellis object.
(
    x, ##<<An object inheriting from class \code{\link{modelFrame}}.
    ... ##<<Further trellis arguments passed to the plotting method.
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

    tmp <- .pull_dot(dots, 'reference', default = NULL)
    reference <- tmp$value
    dots <- tmp$dots

    tmp <- .pull_dot(dots, 'print.trellis', default = TRUE)
    print_trellis <- isTRUE(tmp$value)
    dots <- tmp$dots

    explicit_formula <- NULL
    if (length(dots)) {
        unnamed <- which(!nzchar(names(dots)))
        if (length(unnamed)) {
            k <- unnamed[vapply(dots[unnamed], inherits, logical(1), what = 'formula')]
            if (length(k)) {
                explicit_formula <- dots[[k[1L]]]
                dots[[k[1L]]] <- NULL
            }
        }
    }
    if ('formula' %in% names(dots) && inherits(dots[['formula']], 'formula')) {
        explicit_formula <- dots[['formula']]
        dots[['formula']] <- NULL
    }

    .terminal_kind <- function(object) {
        term <- attr(object, 'terminal', exact = TRUE)
        if (is.character(term) && length(term) == 1L) return(term)
        if (inherits(object, 'frametoFriedman')) return('frametoFriedman')
        if (inherits(object, 'frametoLme')) return('frametoLme')
        if (is.list(object) && !is.null(object$model)) {
            if (inherits(object$model, 'lme')) return('frametoLme')
            if (is.list(object$model) &&
                identical(object$model$method, 'Friedman supersmoother')) {
                return('frametoFriedman')
            }
        }
        'none'
    }

    .group_vars_from_formula <- function(form) {
        if (is.null(form)) return(character(0))
        rhs <- form[[3L]]
        if (is.call(rhs) && identical(rhs[[1L]], as.name('|'))) {
            cond <- rhs[[3L]]
        } else return(character(0))
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
        rhs <- form[[3L]]
        if (is.call(rhs) && identical(rhs[[1L]], as.name('|'))) rhs <- rhs[[2L]]
        vars <- all.vars(rhs)
        if (length(vars)) vars[1L] else character(0)
    }

    .lhs_from_formula <- function(form) {
        vars <- all.vars(form[[2L]])
        if (length(vars)) vars[1L] else character(0)
    }

    .normalize_group <- function(z) {
        if (is.factor(z)) {
            lv <- levels(z)
            num <- suppressWarnings(as.numeric(as.character(lv)))
            if (!anyNA(num)) return(factor(as.character(z), levels = as.character(lv[order(num)])))
            return(z)
        }
        if (is.character(z)) {
            num <- suppressWarnings(as.numeric(z))
            if (!all(is.na(z)) && !anyNA(num[!is.na(z)])) {
                lv <- sort(unique(num[is.finite(num)]))
                return(factor(z, levels = as.character(lv)))
            }
            return(factor(z))
        }
        if (is.logical(z)) return(factor(z))
        if (is.numeric(z)) return(factor(z, levels = sort(unique(z))))
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

    .ensure_panel <- function(dots, reference) {
        if (!is.null(reference) && !('panel' %in% names(dots)) &&
            length(reference) == 1L && is.finite(reference)) {
            ref <- reference
            dots$panel <- function(x, y, ...) {
                lattice::panel.abline(h = ref, lty = 2, lwd = 0.5, col = 'gray30')
                lattice::panel.xyplot(x, y, ...)
            }
        }
        dots
    }

    .build_formnull_fluc <- function(object) {
        fl <- attr(object, 'fluc', exact = TRUE)
        if (!is.null(fl)) return(fl)
        dat <- as.data.frame(object)
        nms <- names(dat)
        low <- tolower(nms)
        ## roles from column types (see cClass)
        time_var <- .BIOdry_time_cols(dat)
        time_var <- if (length(time_var)) time_var[1L] else character(0)
        xnm <- attr(object, 'x.nm', exact = TRUE)
        response <- intersect(as.character(xnm), nms)
        if (!length(response)) {
            pref <- c('x.idx','x.res','x','rw','trw','value','y')
            response <- nms[match(pref, low, nomatch = 0L)]
            response <- response[nzchar(response)]
        }
        if (!length(response)) {
            num <- nms[vapply(dat, is.numeric, logical(1))]
            num <- setdiff(num, time_var)
            response <- if (length(num)) num[1L] else character(0)
        } else response <- response[1L]
        group_vars <- setdiff(.BIOdry_group_cols(dat), c(response, time_var))
        if (length(group_vars)) dat[group_vars] <- lapply(dat[group_vars], .normalize_group)
        if (length(response) && length(time_var)) {
            nlme::groupedData(.build_grouped_formula(response, time_var, group_vars), data = dat)
        } else dat
    }

    kind <- .terminal_kind(x)

    if (identical(kind, 'frametoLme')) {
        mod <- if (inherits(x, 'frametoLme')) x$model else x$model
        if (!is.null(explicit_formula)) {
            tr <- do.call(plot, c(list(x = mod, explicit_formula), dots))
            if (print_trellis && inherits(tr, 'trellis')) print(tr)
            return(invisible(tr))
        }
        dat <- tryCatch(nlme::getData(mod), error = function(e) NULL)
        if (is.null(dat) && !is.null(mod$data)) dat <- mod$data
        if (is.null(dat) && !is.null(x$fluc)) dat <- as.data.frame(x$fluc)
        if (is.null(dat)) stop("Could not recover fitted data from the 'lme' object.", call. = FALSE)
        dat <- as.data.frame(dat)
        dat[['.mf_resid__']] <- as.numeric(resid(mod, type = 'p'))
        dat[['.mf_fitted__']] <- as.numeric(fitted(mod))

        group_vars <- character(0)
        if (!is.null(x$fluc) && inherits(x$fluc, 'groupedData')) {
            group_vars <- .group_vars_from_formula(formula(x$fluc))
        }
        if (!length(group_vars)) {
            gf <- tryCatch(nlme::getGroupsFormula(mod), error = function(e) NULL)
            if (!is.null(gf)) group_vars <- .group_vars_from_formula(gf)
        }
        group_vars <- .select_groups(group_vars, panel_levels)
        if (length(group_vars)) dat[group_vars] <- lapply(dat[group_vars], .normalize_group)

        if (is.null(reference)) reference <- 0
        dots <- .ensure_panel(dots, reference)
        ## fill the panels (nlme's default aspect = "xy" banks them to thin
        ## strips) and label the axes, unless the user gives these
        if (!'aspect' %in% names(dots)) dots$aspect <- 'fill'
        if (!'xlab' %in% names(dots)) dots$xlab <- 'Fitted values'
        if (!'ylab' %in% names(dots)) dots$ylab <- 'Pearson residuals'

        gd <- nlme::groupedData(
            .build_grouped_formula('.mf_resid__', '.mf_fitted__', group_vars),
            data = dat
        )
        tr <- do.call(plot, c(list(x = gd), dots))
        if (print_trellis && inherits(tr, 'trellis')) print(tr)
        return(invisible(tr))
    }

    fl <- NULL
    if (is.list(x) && !is.null(x$fluc)) fl <- x$fluc
    if (is.null(fl)) fl <- attr(x, 'fluc', exact = TRUE)
    if (is.null(fl)) fl <- .build_formnull_fluc(x)

    dat <- as.data.frame(fl)
    form <- if (inherits(fl, 'groupedData')) formula(fl) else NULL

    if (is.null(form)) {
        lhs <- attr(fl, 'mf_response', exact = TRUE)
        xvar <- attr(fl, 'mf_time', exact = TRUE)
        group_vars <- attr(fl, 'mf_groups', exact = TRUE)
    } else {
        lhs <- .lhs_from_formula(form)
        xvar <- .time_from_formula(form)
        group_vars <- .group_vars_from_formula(form)
    }

    if (!length(lhs) || !lhs %in% names(dat)) {
        stop("Could not resolve the fluctuation column to plot.", call. = FALSE)
    }

    if (identical(kind, 'frametoFriedman')) {
        fit_name <- if (grepl('\\.idx$|\\.res$', lhs)) sub('\\.(idx|res)$', '.fit', lhs) else 'x.fit'
        if (!fit_name %in% names(dat)) fit_name <- xvar
        if (is.null(reference)) reference <- if (grepl('\\.idx$', lhs)) 1 else 0
    } else {
        fit_name <- xvar
        if (is.null(reference)) reference <- if (grepl('\\.idx$', lhs)) 1 else 0
    }

    if (!length(fit_name) || !fit_name %in% names(dat)) {
        fit_name <- xvar
    }
    if (!length(fit_name) || !fit_name %in% names(dat)) {
        stop("Could not resolve the x-axis variable for plotting.", call. = FALSE)
    }

    group_vars <- .select_groups(group_vars, panel_levels)
    if (length(group_vars)) dat[group_vars] <- lapply(dat[group_vars], .normalize_group)

    dots <- .ensure_panel(dots, reference)
    ## processed data (form = NULL) and Friedman fits: fill the panels,
    ## unless the user gives aspect; nlme's default aspect = "xy" banks them
    ## to thin strips
    if (!'aspect' %in% names(dots)) dots$aspect <- 'fill'
    ## Friedman fits: label the index and the fitted smooth, unless given
    if (identical(kind, 'frametoFriedman')) {
        diff_idx <- isTRUE(tryCatch(x$model$settings$difference,
                                    error = function(e) NULL)) ||
            grepl('\\.res$', lhs)
        if (!'ylab' %in% names(dots)) {
            dots$ylab <- if (diff_idx) 'Difference index' else 'Ratio index'
        }
        if (!'xlab' %in% names(dots) && !identical(fit_name, xvar)) {
            dots$xlab <- 'Fitted smooth'
        }
    }

    gd <- nlme::groupedData(
        .build_grouped_formula(lhs, fit_name, group_vars),
        data = dat
    )
    tr <- do.call(plot, c(list(x = gd), dots))
    if (print_trellis && inherits(tr, 'trellis')) print(tr)
    invisible(tr)
### A trellis plot object. The object is printed by default.
} , ex=function() {
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
})
