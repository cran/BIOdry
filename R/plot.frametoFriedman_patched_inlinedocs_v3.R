plot.frametoFriedman <- structure(function # Plot Friedman fluctuation series over time
### Trellis plots of detrended Friedman fluctuation series are obtained.
                        ##details<<The default display for
                        ##\code{\link{frametoFriedman}} objects is a
                        ##time-oriented trellis plot of modeled
                        ##fluctuations or residual-like detrended values over
                        ##the resolved time variable. This mimics the
                        ##time-panel displays commonly used with
                        ##\code{nlme} residual objects: the x-axis is the
                        ##time variable, the y-axis is the detrended
                        ##fluctuation column, and panels are defined by the
                        ##declared grouping hierarchy.
                        ##
                        ##When \code{x$fluc} inherits from
                        ##\code{\link[nlme]{groupedData}}, its stored
                        ##formula is honored if it already has the form
                        ##\code{fluctuation ~ time | groups}. If that formula
                        ##cannot be used safely, the method reconstructs a
                        ##temporary grouped object with the fluctuation on the
                        ##left-hand side, the time variable on the x-axis, and
                        ##the selected grouping variables in the conditioning
                        ##structure.
                        ##
                        ##Numeric-like grouping variables are normalized to
                        ##natural numeric order in the plotting copy only, so
                        ##panel strips such as zones appear in the order
                        ##\code{1, 2, 3, ...} instead of first appearance.
(
    x, ##<<Object inheriting from class \code{\link{frametoFriedman}}.
    ..., ##<<Further trellis arguments passed to the plotting method.
    levels = NULL, ##<<Optional subset of grouping variables used for
                   ##conditioning. May be numeric positions or
                   ##character names.
    print.trellis = TRUE ##<<\code{logical}. Print the trellis object.
) {
    if (is.null(x$fluc)) {
        stop("This 'frametoFriedman' object has no 'fluc' component to plot.", call. = FALSE)
    }

    dots <- list(...)

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

    .lhs_from_formula <- function(form) {
        vars <- all.vars(form[[2L]])
        if (length(vars)) vars[1L] else character(0)
    }

    .time_from_formula <- function(form) {
        rhs <- form[[3L]]
        if (is.call(rhs) && identical(rhs[[1L]], as.name('|'))) rhs <- rhs[[2L]]
        vars <- all.vars(rhs)
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
                lev <- as.character(sort(unique(num[is.finite(num)])))
                return(factor(z, levels = lev))
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

    fl <- x$fluc
    dat <- as.data.frame(fl)
    form <- if (inherits(fl, 'groupedData')) formula(fl) else NULL

    yvar <- attr(fl, 'mf_response', exact = TRUE)
    xvar <- attr(fl, 'mf_time', exact = TRUE)
    group_vars <- attr(fl, 'mf_groups', exact = TRUE)

    if ((!length(yvar) || !length(xvar)) && !is.null(form)) {
        if (!length(yvar)) yvar <- .lhs_from_formula(form)
        if (!length(xvar)) xvar <- .time_from_formula(form)
        if (!length(group_vars)) group_vars <- .group_vars_from_formula(form)
    }

    if (!length(yvar) || !yvar %in% names(dat)) {
        pref <- grep('\\.(idx|res)$', names(dat), value = TRUE)
        yvar <- if (length(pref)) pref[1L] else character(0)
    }
    if (!length(yvar) || !yvar %in% names(dat)) {
        stop("Could not resolve the detrended fluctuation column to plot.", call. = FALSE)
    }

    if (!length(xvar) || !xvar %in% names(dat)) {
        ## first whole-number column (see cClass)
        hit <- .BIOdry_time_cols(dat)
        xvar <- if (length(hit)) hit[1L] else character(0)
    }
    if (!length(xvar) || !xvar %in% names(dat)) {
        stop("Could not resolve the time variable for plotting.", call. = FALSE)
    }

    if (!length(group_vars)) {
        group_vars <- setdiff(names(dat), c(yvar, xvar, sub('\\.(idx|res)$', '.fit', yvar), sub('\\.(idx|res)$', '', yvar)))
        group_vars <- group_vars[vapply(dat[group_vars], function(z) is.factor(z) || is.character(z) || is.logical(z) || is.numeric(z), logical(1))]
    }
    all_groups <- group_vars
    group_vars <- .select_groups(group_vars, levels)
    if (length(group_vars)) dat[group_vars] <- lapply(dat[group_vars], .normalize_group)
    ## levels coarser than the series: one line per series within each
    ## panel, so lines never join different series
    dropped <- setdiff(all_groups, group_vars)

    keep_vars <- unique(c(yvar, xvar, group_vars, dropped))
    keep <- stats::complete.cases(dat[, keep_vars, drop = FALSE])
    dat <- dat[keep, , drop = FALSE]
    if (!nrow(dat)) {
        stop('No complete cases available for time-oriented fluctuation plotting.', call. = FALSE)
    }
    if (length(group_vars)) {
        dat[group_vars] <- lapply(dat[group_vars], function(z) if (is.factor(z)) droplevels(z) else z)
    }

    if (!('as.table' %in% names(dots))) dots$as.table <- TRUE
    if (!('type' %in% names(dots))) dots$type <- c('p','l')
    ## fill the panels (nlme's default aspect = "xy" banks them to thin
    ## strips) and label the index, unless the user gives these
    if (!('aspect' %in% names(dots))) dots$aspect <- 'fill'
    if (!('ylab' %in% names(dots))) {
        diff_idx <- isTRUE(tryCatch(x$model$settings$difference,
                                    error = function(e) NULL)) ||
            grepl('\\.res$', yvar)
        dots$ylab <- if (diff_idx) 'Difference index' else 'Ratio index'
    }

    if (length(dropped) && !any(c('inner', 'groups') %in% names(dots))) {
        ## one line per series within each panel (nlme's inner = draws an
        ## extra line through all points of a panel, so lattice is used)
        series <- interaction(dat[dropped], drop = TRUE, sep = '/')
        o <- order(series, dat[[xvar]])
        dat <- dat[o, , drop = FALSE]
        series <- series[o]
        fm <- stats::as.formula(paste(yvar, '~', xvar,
                                      if (length(group_vars))
                                          paste('|', paste(group_vars, collapse = ' * '))
                                      else ''))
        if (!('auto.key' %in% names(dots))) {
            dots$auto.key <- list(columns = min(4L, nlevels(series)),
                                  lines = TRUE, points = FALSE)
        }
        tr <- do.call(lattice::xyplot,
                      c(list(x = fm, data = dat, groups = series), dots))
    } else {
        gd <- nlme::groupedData(.build_grouped_formula(yvar, xvar, group_vars), data = dat)
        tr <- do.call(plot, c(list(x = gd), dots))
    }
    if (isTRUE(print.trellis) && inherits(tr, 'trellis')) print(tr)
    invisible(tr)
### A trellis plot object. The object is printed by default.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    ff <- frametoFriedman(dfm2)
    ## ratio index of each core over time
    plot(ff)
    ## panels by plot, one line per core
    plot(ff, levels = 'plot', layout = c(2, 1))
})
