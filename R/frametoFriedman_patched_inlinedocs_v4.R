## roles from column types (see cClass): whole-number columns are time
## candidates; factor and character columns that are not months are groups
.friedman_guess_time_vars <- function(dat) {
    .BIOdry_time_cols(dat)
}

.friedman_guess_group_vars <- function(dat, time_var = character(0), response = character(0)) {
    setdiff(.BIOdry_group_cols(dat), c(time_var, response))
}

.friedman_guess_response <- function(dat, time_var = character(0), group_vars = character(0)) {
    if (!is.data.frame(dat) || !ncol(dat)) return(character(0))

    nms <- names(dat)
    low <- tolower(nms)

    from_attr <- attr(dat, 'x.nm', exact = TRUE)
    if (!is.null(from_attr)) {
        hit <- intersect(as.character(from_attr), nms)
        if (length(hit)) return(hit[1L])
    }

    pref <- c('ai', 'x', 'rw', 'trw', 'ring', 'ring_width', 'ringwidth', 'value', 'y')
    hit <- nms[match(pref, low, nomatch = 0L)]
    hit <- hit[nzchar(hit)]
    hit <- setdiff(hit, c(time_var, group_vars))
    if (length(hit)) return(hit[1L])

    num <- nms[vapply(dat, is.numeric, logical(1))]
    num <- setdiff(num, c(time_var, group_vars))
    if (length(num)) return(num[1L])

    character(0)
}

.friedman_time_numeric <- function(x) {
    if (inherits(x, 'Date')) return(as.numeric(x))
    if (inherits(x, 'POSIXt')) return(as.numeric(x))

    if (is.factor(x)) x <- as.character(x)

    if (is.character(x)) {
        suppressWarnings(nx <- as.numeric(x))
        if (sum(is.finite(nx)) >= sum(!is.na(x))) return(nx)

        xs <- tolower(substr(trimws(x), 1L, 3L))
        out <- match(xs, tolower(month.abb))
        miss <- is.na(out)
        if (any(miss)) {
            out[miss] <- match(xs[miss], tolower(substr(month.name, 1L, 3L)))
        }
        return(as.numeric(out))
    }

    if (is.logical(x)) return(as.numeric(x))
    as.numeric(x)
}

.friedman_build_grouped_formula <- function(resp, time_var, groups) {
    rhs <- time_var
    if (length(groups)) rhs <- paste(rhs, '|', paste(groups, collapse = '/'))
    formula(paste(resp, '~', rhs))
}

.friedman_validate_span <- function(span) {
    if (is.character(span) && length(span) == 1L && identical(span, 'cv')) return(span)
    if (is.numeric(span) && length(span) == 1L && is.finite(span) && span > 0 && span <= 1) {
        return(as.numeric(span))
    }
    stop("'span' must be 'cv' or a single numeric value in (0, 1].", call. = FALSE)
}

.friedman_validate_bass <- function(bass) {
    bass <- as.numeric(bass)[1L]
    if (!is.finite(bass) || is.na(bass) || bass < 0) {
        stop("'bass' must be a single non-negative numeric value.", call. = FALSE)
    }
    bass
}

.friedman_split_index <- function(dat, group_vars) {
    if (!length(group_vars)) return(list(seq_len(nrow(dat))))
    split(seq_len(nrow(dat)), interaction(dat[group_vars], drop = TRUE, lex.order = TRUE))
}

.friedman_norm_group <- function(x) {
    if (is.factor(x) || is.character(x)) {
        xn <- suppressWarnings(as.numeric(as.character(x)))
        if (!all(is.na(xn))) {
            lev <- as.character(sort(unique(xn[!is.na(xn)])))
            return(factor(as.character(x), levels = lev))
        }
        return(factor(x))
    }
    if (is.numeric(x) || is.integer(x)) {
        lev <- sort(unique(x[!is.na(x)]))
        return(factor(x, levels = lev))
    }
    x
}

.friedman_finish_fluc <- function(dat, det_name, time_var, group_vars, meta = NULL) {
    dat <- dat[order(dat[['.orig_row_id__']]), , drop = FALSE]
    dat[['.orig_row_id__']] <- NULL

    if (identical(time_var, '.friedman_time__')) {
        dat[['.friedman_time__']] <- NULL
        time_var <- character(0)
    }

    if (length(group_vars)) {
        for (g in group_vars) dat[[g]] <- .friedman_norm_group(dat[[g]])
    }

    attr(dat, 'time_cols') <- time_var
    attr(dat, 'split_vars') <- group_vars

    if (length(time_var) == 1L) {
        fl_form <- .friedman_build_grouped_formula(det_name, time_var[1L], group_vars)
        gd <- groupedData(fl_form, data = dat)
        if (!is.null(meta)) attr(gd, 'BIOdry_meta') <- meta
        return(gd)
    }

    if (!is.null(meta)) attr(dat, 'BIOdry_meta') <- meta
    dat
}

.friedman_fit_one_series <- function(dat, response, time_num,
                                     fit_name, det_name,
                                     difference, span, bass,
                                     periodic, min.obs,
                                     validate, signal_fun) {
    y <- dat[[response]]
    ok <- is.finite(y) & is.finite(time_num)

    out_fit <- rep(NA_real_, length(y))
    out_det <- rep(NA_real_, length(y))

    info <- list(
        n_total = length(y),
        n_used = sum(ok),
        n_unique_time = NA_integer_,
        status = 'ok',
        note = ''
    )

    if (sum(ok) < min.obs) {
        info$status <- 'skipped'
        info$note <- sprintf('fewer than %d finite observations', min.obs)
        dat[[fit_name]] <- out_fit
        dat[[det_name]] <- out_det
        return(list(data = dat, info = info))
    }

    xx <- time_num[ok]
    yy <- y[ok]
    oo <- order(xx, method = 'radix')
    xx <- xx[oo]
    yy <- yy[oo]

    if (anyDuplicated(xx)) {
        agg <- stats::aggregate(list(y = yy), by = list(x = xx), FUN = mean)
        xu <- agg[['x']]
        yu <- agg[['y']]
    } else {
        xu <- xx
        yu <- yy
    }
    info$n_unique_time <- length(xu)

    if (length(xu) < min.obs) {
        info$status <- 'skipped'
        info$note <- sprintf('fewer than %d unique time points', min.obs)
        dat[[fit_name]] <- out_fit
        dat[[det_name]] <- out_det
        return(list(data = dat, info = info))
    }

    span_use <- if (is.character(span) && identical(span, 'cv')) 0 else span
    fit <- tryCatch(
        stats::supsmu(x = xu, y = yu, span = span_use, bass = bass, periodic = periodic),
        error = function(e) e
    )

    if (inherits(fit, 'error')) {
        info$status <- 'failed'
        info$note <- conditionMessage(fit)
        if (!identical(validate, 'none')) {
            signal_fun(sprintf('Friedman smoother failed on one series: %s', info$note))
        }
        dat[[fit_name]] <- out_fit
        dat[[det_name]] <- out_det
        return(list(data = dat, info = info))
    }

    f_ok <- stats::approx(x = fit$x, y = fit$y, xout = time_num[ok], rule = 2, ties = 'ordered')$y
    out_fit[ok] <- f_ok

    if (difference) {
        out_det[ok] <- y[ok] - f_ok
    } else {
        good_den <- is.finite(f_ok) & (f_ok > 0)
        ok_idx <- which(ok)
        out_det[ok_idx[good_den]] <- y[ok_idx[good_den]] / f_ok[good_den]
        bad_den <- ok_idx[!good_den]
        if (length(bad_den)) {
            info$note <- 'non-positive fitted values produced NA ratio detrending'
            if (!identical(validate, 'none')) {
                signal_fun('Some fitted values were non-positive; ratio detrending was set to NA for those positions.')
            }
        }
    }

    dat[[fit_name]] <- out_fit
    dat[[det_name]] <- out_det
    list(data = dat, info = info)
}

.frametoFriedman_summary_core <- function(object) {
    md <- object$model
    list(
        call = object$call,
        method = md$method,
        response = md$response,
        time = md$time,
        groups = md$groups,
        nobs = md$nobs,
        n_series = md$n_series,
        successful_series = md$successful_series,
        skipped_series = md$skipped_series,
        failed_series = md$failed_series,
        span = md$settings$span,
        bass = md$settings$bass,
        periodic = md$settings$periodic,
        difference = md$settings$difference,
        min_obs = md$settings$min.obs,
        has_fluc = !is.null(object$fluc),
        series = md$series
    )
}

frametoFriedman <- structure(function # Friedman supersmoother detrending
### Friedman smoothers are fitted to multilevel ecological series.
                        ##details<<This terminal fitter applies
                        ##\code{\link[stats]{supsmu}} series by series on a
                        ##multilevel long-format data frame. It resolves one
                        ##response variable, one primary time variable, and a
                        ##grouping hierarchy defining the independent series.
                        ##
                        ##This patched version is constructor-aware. When a
                        ##\code{BIOdry_meta} attribute is available, the
                        ##resolved time variable is excluded from the Friedman
                        ##grouping hierarchy unless time was explicitly
                        ##declared as part of groups. This prevents accidental
                        ##smoothing of one-point series such as
                        ##\code{zone / hydro_year_id} when the intended fit
                        ##is one series per \code{zone} over
                        ##\code{hydro_year_id}.
                        ##
                        ##The returned object is still a Friedman detrending
                        ##object, not a mixed model. If \code{res.data =
                        ##TRUE}, component \code{$fluc} contains detrended
                        ##series and fitted values for plotting and later
                        ##second-stage analyses.
                        ##references<<Friedman, J. H. 1984. A variable span
                        ##smoother. Laboratory for Computational Statistics,
                        ##Stanford University Technical Report 5.
(
    rd, ##<<\code{data.frame}. Multilevel ecological data series.
    form = 'FriedmanForm', ##<<\code{character}, \code{list}, or
                           ##\code{NULL}. Terminal smoother
                           ##specification.
    response = NULL, ##<<\code{NULL} or \code{character}. Response series.
    time = NULL, ##<<\code{NULL} or \code{character}. Time variable.
    groups = NULL, ##<<\code{NULL} or \code{character}. Grouping variables.
    res.data = TRUE, ##<<\code{logical}. If \code{TRUE}, attach detrended
                     ##series in component \code{fluc}.
    difference = NULL, ##<<\code{NULL} or \code{logical}. Use subtraction
                       ##instead of ratio detrending.
    span = NULL, ##<<\code{NULL}, \code{character}, or \code{numeric}. Span.
    bass = NULL, ##<<\code{NULL} or \code{numeric}. Bass argument.
    periodic = NULL, ##<<\code{NULL} or \code{logical}. Periodic option.
    min.obs = NULL, ##<<\code{NULL} or \code{integer}. Minimum finite
                    ##observations per series.
    validate = c('none', 'warn', 'stop'), ##<<\code{character}. Validation mode.
    ... ##<<Unused extra arguments kept for compatibility.
) {
    validate <- match.arg(validate)

    .signal <- function(msg) {
        if (identical(validate, 'stop')) stop(msg, call. = FALSE)
        if (identical(validate, 'warn')) warning(msg, call. = FALSE)
        invisible(NULL)
    }
    `%||%` <- function(x, y) if (is.null(x)) y else x

    if (!is.data.frame(rd)) stop("'rd' must be a data.frame.", call. = FALSE)
    if (!is.logical(res.data) || length(res.data) != 1L || is.na(res.data)) {
        stop("'res.data' must be TRUE or FALSE.", call. = FALSE)
    }

    meta <- attr(rd, 'BIOdry_meta', exact = TRUE)

    spec <- NULL
    if (inherits(form, 'FriedmanForm')) {
        spec <- form
    } else if (is.character(form) && length(form) == 1L) {
        if (tolower(form) == 'friedmanform') {
            spec <- FriedmanForm(rd, validate = validate)
        } else if (!grepl('~', form, fixed = TRUE) && !is.null(form)) {
            .signal('Unknown Friedman form helper; falling back to direct arguments and automatic guessing.')
        }
    } else if (is.list(form) && !is.null(names(form))) {
        spec <- form
    }

    response <- response %||% spec[['response']]
    time <- time %||% spec[['time']]
    groups <- groups %||% spec[['groups']]
    difference <- difference %||% spec[['difference']] %||% FALSE
    span <- span %||% spec[['span']] %||% 'cv'
    bass <- bass %||% spec[['bass']] %||% 0
    periodic <- periodic %||% spec[['periodic']] %||% FALSE
    min.obs <- min.obs %||% spec[['min.obs']] %||% 4L
    time_in_groups <- isTRUE(spec[['time_in_groups']]) || isTRUE(meta$time_in_groups)

    if (!is.logical(difference) || length(difference) != 1L || is.na(difference)) {
        stop("'difference' must be TRUE or FALSE.", call. = FALSE)
    }
    if (!is.logical(periodic) || length(periodic) != 1L || is.na(periodic)) {
        stop("'periodic' must be TRUE or FALSE.", call. = FALSE)
    }
    span <- .friedman_validate_span(span)
    bass <- .friedman_validate_bass(bass)
    min.obs <- as.integer(min.obs)[1L]
    if (!is.finite(min.obs) || is.na(min.obs) || min.obs < 2L) {
        stop("'min.obs' must be an integer >= 2.", call. = FALSE)
    }

    guess_time <- .friedman_guess_time_vars(rd)
    if (is.null(time)) {
        time_candidates <- guess_time
    } else {
        time_candidates <- intersect(as.character(time), names(rd))
        if (!length(time_candidates)) stop("The requested 'time' column was not found in 'rd'.", call. = FALSE)
    }
    if (!length(time_candidates) && .BIOdry_declared(meta, 'time')) {
        time_candidates <- intersect(unique(c(meta$time_vars, meta$time)), names(rd))
    }
    time_var <- .Friedman_choose_time(time_candidates, names(rd))

    if (is.null(groups)) {
        group_vars <- .Friedman_split_group_spec(spec[['groups']] %||% NULL)
        if (is.null(group_vars) || !length(group_vars)) {
            if (.BIOdry_declared(meta, 'groups') && length(meta$groups)) {
                group_vars <- .Friedman_split_group_spec(meta$groups)
            } else {
                group_vars <- .friedman_guess_group_vars(rd, time_var = time_var)
            }
        }
    } else {
        group_vars <- .Friedman_split_group_spec(groups)
        group_vars <- intersect(as.character(group_vars), names(rd))
        if (!length(group_vars)) group_vars <- character(0)
    }

    if (!time_in_groups) {
        group_vars <- setdiff(group_vars, unique(c(time_var, time_candidates)))
    }
    group_vars <- unique(group_vars)

    if (is.null(response)) {
        if (.BIOdry_declared(meta, 'response') && length(meta$active_response %||% meta$response)) {
            response <- (meta$active_response %||% meta$response)[1L]
        } else {
            response <- .friedman_guess_response(rd, time_var = time_var, group_vars = group_vars)
        }
    }
    response <- intersect(as.character(response), names(rd))
    if (!length(response)) stop('Could not resolve a numeric response variable to smooth.', call. = FALSE)
    response <- response[1L]
    if (!is.numeric(rd[[response]])) stop("The selected 'response' column must be numeric.", call. = FALSE)

    if (!length(time_var)) {
        .signal('No explicit time column was detected; using within-series row order.')
        split_id <- if (length(group_vars)) interaction(rd[group_vars], drop = TRUE, lex.order = TRUE) else factor(rep('.all', nrow(rd)))
        rd[['.friedman_time__']] <- ave(seq_len(nrow(rd)), split_id, FUN = seq_along)
        time_var <- '.friedman_time__'
    }

    time_num <- .friedman_time_numeric(rd[[time_var]])
    if (!any(is.finite(time_num))) stop("The selected 'time' column could not be converted to numeric values.", call. = FALSE)

    ord_vars <- unique(c(group_vars, time_var))
    ord_df <- lapply(ord_vars, function(nm) {
        x <- rd[[nm]]
        if (identical(nm, time_var)) return(time_num)
        if (is.factor(x)) as.character(x) else x
    })
    ord <- do.call(order, c(ord_df, list(na.last = TRUE, method = 'radix')))
    rd_ord <- rd[ord, , drop = FALSE]
    time_num_ord <- time_num[ord]
    rd_ord[['.orig_row_id__']] <- ord

    split_idx <- .friedman_split_index(rd_ord, group_vars)
    ## the series in the order of their first rows in the data
    first_row <- vapply(split_idx, function(i) min(rd_ord[['.orig_row_id__']][i]),
                        numeric(1L))
    split_idx <- split_idx[order(first_row)]
    fit_name <- paste(response, 'fit', sep = '.')
    det_name <- paste(response, if (difference) 'res' else 'idx', sep = '.')

    out_blocks <- vector('list', length(split_idx))
    series_meta <- vector('list', length(split_idx))
    ok_series <- skipped_series <- failed_series <- 0L

    for (i in seq_along(split_idx)) {
        idx <- split_idx[[i]]
        subi <- rd_ord[idx, , drop = FALSE]
        resi <- .friedman_fit_one_series(
            dat = subi,
            response = response,
            time_num = time_num_ord[idx],
            fit_name = fit_name,
            det_name = det_name,
            difference = difference,
            span = span,
            bass = bass,
            periodic = periodic,
            min.obs = min.obs,
            validate = validate,
            signal_fun = .signal
        )

        out_blocks[[i]] <- resi$data

        meta_i <- as.list(if (length(group_vars)) subi[1L, group_vars, drop = FALSE] else data.frame(series = '.all', stringsAsFactors = FALSE))
        meta_i$n_total <- resi$info$n_total
        meta_i$n_used <- resi$info$n_used
        meta_i$n_unique_time <- resi$info$n_unique_time
        meta_i$status <- resi$info$status
        meta_i$note <- resi$info$note
        meta_i$n_na <- sum(is.na(resi$data[[det_name]]))
        series_meta[[i]] <- as.data.frame(meta_i, stringsAsFactors = FALSE)

        if (identical(resi$info$status, 'ok')) ok_series <- ok_series + 1L
        if (identical(resi$info$status, 'skipped')) skipped_series <- skipped_series + 1L
        if (identical(resi$info$status, 'failed')) failed_series <- failed_series + 1L
    }

    smooth_dat <- do.call(rbind, out_blocks)
    series_meta <- do.call(rbind, series_meta)
    rownames(series_meta) <- NULL

    fl_cols <- unique(c(det_name, fit_name, response, time_var, group_vars, '.orig_row_id__'))
    fl_dat <- smooth_dat[, fl_cols, drop = FALSE]

    meta_out <- if (is.list(meta)) meta else list()
    meta_out$active_response <- response
    meta_out$time <- if (identical(time_var, '.friedman_time__')) NULL else time_var
    meta_out$time_vars <- unique(c(meta_out$time_vars, meta_out$time))
    meta_out$groups <- group_vars
    meta_out$time_in_groups <- isTRUE(time_in_groups)
    class(meta_out) <- c('BIOdry_meta', 'list')

    fluc <- if (res.data) {
        .friedman_finish_fluc(fl_dat, det_name = det_name, time_var = time_var, group_vars = group_vars, meta = meta_out)
    } else NULL

    if (identical(time_var, '.friedman_time__')) {
        smooth_dat[['.friedman_time__']] <- NULL
        if ('series' %in% names(series_meta)) series_meta[['series']] <- NULL
        time_var <- character(0)
    }

    mem <- list(
        model = list(
            method = 'Friedman supersmoother',
            response = response,
            time = time_var,
            groups = group_vars,
            nobs = nrow(rd),
            n_series = nrow(series_meta),
            successful_series = ok_series,
            skipped_series = skipped_series,
            failed_series = failed_series,
            settings = list(
                difference = difference,
                span = span,
                bass = bass,
                periodic = periodic,
                min.obs = min.obs,
                form = if (inherits(spec, 'FriedmanForm')) spec else NULL
            ),
            series = series_meta
        ),
        call = sys.call()
    )
    class(mem) <- c('frametoFriedman', class(mem))
    if (res.data) mem[['fluc']] <- fluc
    attr(mem, 'BIOdry_meta') <- meta_out
    mem
### \code{list} of class \code{"frametoFriedman"} with at least
### components \code{model} and \code{call}, and optionally
### \code{fluc}.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    ## Friedman supersmoother on each core; ratio index x / smooth
    ff <- frametoFriedman(dfm2)
    ff
    head(ff$fluc)
    ## difference index x - smooth
    frametoFriedman(dfm2, difference = TRUE)$fluc |> head()
})

summary.frametoFriedman <- structure(function # Summarize frametoFriedman objects
### A compact summary of a \code{\link{frametoFriedman}} object is obtained.
                        ##details<<This method extracts the main
                        ##components that users typically inspect after
                        ##Friedman detrending: the original call, the chosen
                        ##response and time variables, the series hierarchy,
                        ##the smoothing settings, the number of observations
                        ##and series, and a per-series status table.
(
    object, ##<<Object inheriting from class \code{"frametoFriedman"}.
    ... ##<<Unused extra arguments.
) {
    out <- .frametoFriedman_summary_core(object)
    class(out) <- c('summary.frametoFriedman', class(out))
    out
### A compact summary object of class \code{"summary.frametoFriedman"}.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    s <- dfm2 |> frametoFriedman() |> summary()
    ## settings and the status of each series
    s$span
    s$series
})

print.summary.frametoFriedman <- structure(function # Print compact frametoFriedman summaries
### A compact printed summary of a \code{\link{frametoFriedman}} object is obtained.
(
    x, ##<<Object inheriting from class \code{"summary.frametoFriedman"}.
    digits = max(3L, getOption('digits') - 2L), ##<<Integer number of digits.
    ... ##<<Unused extra arguments.
) {
    cat('Friedman supersmoother detrending\n\n')

    cat('Call:\n')
    print(x$call)

    cat('\nData and settings:\n')
    cat('  response :', x$response, '\n')
    cat('  time     :', if (length(x$time)) x$time else '<within-series order>', '\n')
    cat('  groups   :', if (length(x$groups)) paste(x$groups, collapse = ' / ') else '<none>', '\n')
    cat('  nobs     :', x$nobs, '\n')
    cat('  n_series :', x$n_series, '\n')
    cat('  success  :', x$successful_series, '\n')
    cat('  skipped  :', x$skipped_series, '\n')
    cat('  failed   :', x$failed_series, '\n')
    cat('  span     :', format(x$span), '\n')
    cat('  bass     :', format(signif(x$bass, digits = digits)), '\n')
    cat('  periodic :', x$periodic, '\n')
    cat('  difference:', x$difference, '\n')
    cat('  min.obs  :', x$min_obs, '\n')

    if (is.data.frame(x$series) && nrow(x$series)) {
        cat('\nSeries status (first rows):\n')
        st <- utils::head(x$series, 10L)
        print(st[, setdiff(names(st), c('note', 'n_na')), drop = FALSE],
              row.names = FALSE)
        if (nrow(x$series) > 10L) cat(sprintf('... %d more series not shown.\n', nrow(x$series) - 10L))
        ## notes below the table, one line per series with a note
        has_note <- !is.na(x$series$note) & nzchar(x$series$note)
        if (any(has_note)) {
            gv <- intersect(x$groups, names(x$series))
            if (!length(gv)) gv <- setdiff(names(x$series), c('n_total', 'n_used', 'n_unique_time', 'status', 'note', 'n_na'))
            cat('\nNotes:\n')
            for (k in which(has_note)) {
                lab <- paste(vapply(gv, function(g) as.character(x$series[[g]][k]), ''), collapse = '/')
                n_na <- x$series$n_na[k]
                note_k <- x$series$note[k]
                ## short form within 80 columns; the full note stays in
                ## the series table
                txt <- if (grepl('non-positive fitted values', note_k, fixed = TRUE) &&
                           length(n_na) && !is.na(n_na)) {
                    sprintf('%d NA ratio %s (non-positive fitted values)', n_na,
                            if (n_na == 1L) 'index' else 'indices')
                } else if (length(n_na) && !is.na(n_na) && n_na > 0L) {
                    sprintf('%s (%d NA)', note_k, n_na)
                } else note_k
                cat(sprintf('  %s: %s\n', lab, txt))
            }
        }
    }

    if (isTRUE(x$has_fluc)) cat('\nDetrended series are available in $fluc.\n')
    invisible(x)
### The printed compact summary.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    dfm2 |> frametoFriedman() |> summary() |> print()
})

print.frametoFriedman <- structure(function # Print frametoFriedman objects
### A compact printed representation of a \code{\link{frametoFriedman}} object is obtained.
                        ##details<<This method prints the compact summary
                        ##instead of the raw list returned by
                        ##\code{\link{frametoFriedman}}.
(
    x, ##<<Object inheriting from class \code{"frametoFriedman"}.
    ... ##<<Additional arguments passed to \code{\link{summary.frametoFriedman}}.
) {
    sx <- summary.frametoFriedman(x, ...)
    print.summary.frametoFriedman(sx, ...)
    invisible(x)
### The compact printed representation.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    ## printing shows the compact summary, not the nested list
    dfm2 |> frametoFriedman() |> print()
})
