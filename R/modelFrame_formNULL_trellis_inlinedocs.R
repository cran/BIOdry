modelFrame <- structure(function # Recursive dispatch of multilevel series functions
### This function recursively dispatches user-defined stage functions
### on multilevel long-format series and can optionally apply a
### terminal fitter specified by 'form'.
                        ##details<< By default the workflow applies
                        ##\code{\link{rtimes}},
                        ##\code{\link{scacum}}, and
                        ##\code{\link{amod}} through
                        ##\code{\link{ringApply}}, then uses
                        ##\code{\link{tdForm}} as the terminal model
                        ##specification for \code{\link{frametoLme}}.
                        ##More generally, \code{modelFrame} behaves as a
                        ##recursive dispatcher for multilevel ecological
                        ##series. Functions listed in \code{fn} are
                        ##evaluated stage by stage on the hierarchy
                        ##defined by \code{lv}. The terminal path is now
                        ##driven by \code{form}: \code{NULL} returns the
                        ##processed long-format \code{data.frame};
                        ##\code{\link{tdForm}}, \code{\link{lmeForm}},
                        ##an explicit formula, or a formula-like
                        ##character string trigger
                        ##\code{\link{frametoLme}}; and
                        ##\code{\link{FriedmanForm}} triggers
                        ##\code{\link{frametoFriedman}}. For backward
                        ##compatibility, a terminal fitter placed as the
                        ##last entry of \code{fn} is still recognized,
                        ##but the preferred grammar is now to keep
                        ##\code{fn} for recursive stage functions and
                        ##\code{form} for the terminal model or smoother
                        ##specification. The implementation is
                        ##conservative with multilevel data: it prefers
                        ##explicit time columns over row names, reshapes
                        ##only when the input clearly looks wide,
                        ##restricts unit conversion to record-like
                        ##numeric columns, resolves numeric
                        ##\code{lv} positions against the current
                        ##hierarchy at each stage, and validates the
                        ##stage contract. In the terminal
                        ##\code{\link{frametoLme}} fit, rows with a
                        ##missing value in the fitted variables or the
                        ##grouping columns are dropped without a message;
                        ##with the default \code{\link{tdForm}} these are
                        ##the rows where \code{\link{rtimes}} gives no
                        ##time (16 of the 1008 rows of \code{Pchron}).
                        ##Arguments for \code{\link[nlme]{lme}}, such as
                        ##\code{random = pdDiag(~ log(csx))} with a
                        ##formula grouped by tree (a tree intercept and a
                        ##tree-level \code{log(csx)} slope, as in the
                        ##\code{\link{frametoLme}} example), are passed on
                        ##to \code{\link{frametoLme}}. Arguments in
                        ##\code{modelFrame} objects can be updated with
                        ##\code{\link{update}}. Use
                        ##\code{validate = "warn"} or
                        ##\code{validate = "stop"} for stricter checks.

                        ##references<< Lara W., F. Bravo,
                        ##D. Maguire. 2013. Modeling patterns between
                        ##drought and tree biomass growth from
                        ##dendrochronological data: A multilevel
                        ##approach. Agric. For. Meteorol.,
                        ##178-179:140-151.
(
    rd, ##<<\code{data.frame} or \code{list}. Dendroclimatic
        ##chronology or multilevel ecological data series.
    fn = list('rtimes','scacum','amod'), ##<<\code{list}. Names of
                                         ##recursive stage functions to
                                         ##dispatch through
                                         ##\code{\link{ringApply}}.
                                         ##Stage functions must return a
                                         ##\code{data.frame}. The
                                         ##preferred grammar keeps
                                         ##terminal fitting out of
                                         ##\code{fn}; however, a final
                                         ##\code{\link{frametoLme}} or
                                         ##\code{\link{frametoFriedman}}
                                         ##entry is still recognized for
                                         ##backward compatibility.

    lv = list(2,1,1), ##<<\code{list}. Splitting levels for the
                      ##functions that are evaluated through
                      ##\code{\link{ringApply}}. Numeric values are
                      ##resolved against the current hierarchy at each
                      ##stage. Character values are interpreted as
                      ##grouping columns in the current data. If a
                      ##terminal fitter is supplied as the last entry of
                      ##\code{fn} for backward compatibility,
                      ##\code{lv} applies only to the preceding stage
                      ##functions.
    form = 'tdForm', ##<<\code{NULL}, \code{character},
                     ##\code{formula}, or \code{list}. Terminal model
                     ##specification. \code{NULL} returns only the
                     ##processed long data. \code{\link{tdForm}},
                     ##\code{\link{lmeForm}}, a formula, or a
                     ##formula-like character string trigger
                     ##\code{\link{frametoLme}}. \code{\link{FriedmanForm}}
                     ##or an object inheriting from class
                     ##\code{"FriedmanForm"} trigger
                     ##\code{\link{frametoFriedman}}.
    validate = c('none','warn','stop'), ##<<\code{character}. Validation mode.
    ... ##<<Further arguments in \code{\link{mUnits}}, the
        ##recursive stage functions, the terminal fitter, or the
        ##terminal specification helper named in \code{form}.
) {
    validate <- match.arg(validate)
    .emit <- function(msg) {
        if (identical(validate, 'stop')) stop(msg, call. = FALSE)
        if (identical(validate, 'warn')) warning(msg, call. = FALSE)
        invisible(NULL)
    }
    .as_stage_list <- function(x) {
        if (is.list(x)) return(x)
        as.list(x)
    }
    .recycle_stage <- function(x, n, nm) {
        len <- length(x)
        if (len == n) return(x)
        if (len == 1L) return(rep(x, n))
        .emit(sprintf("Length of '%s' (%d) does not match number of stages (%d); recycling with rep_len().",
                      nm, len, n))
        rep_len(x, n)
    }
    ## roles from column types (see cClass): whole-number columns are time
    ## candidates; factor and character columns that are not months are groups
    .time_cols <- function(d) .BIOdry_time_cols(d)
    .id_like_cols <- function(d) .BIOdry_group_cols(d)
    .measure_cols <- function(d) {
        if (!is.data.frame(d)) return(character(0))
        xnm <- attr(d, 'x.nm', exact = TRUE)
        if (!is.null(xnm)) {
            xnm <- intersect(as.character(xnm), names(d))
        } else xnm <- character(0)
        num <- names(d)[vapply(d, is.numeric, logical(1))]
        exc <- unique(c(.time_cols(d), .id_like_cols(d)))
        out <- setdiff(num, exc)
        if (length(xnm)) out <- unique(c(xnm, out))
        out
    }
    .looks_wide <- function(d) {
        if (!is.data.frame(d)) return(FALSE)
        if (inherits(d, c('groupedData','nmGroupedData','modelFrame'))) return(FALSE)
        atn <- names(attributes(d))
        if (all(c('x.nm','f.nm','t.nm','time_cols') %in% atn)) return(TRUE)
        tcols <- .time_cols(d)
        chars <- names(d)[vapply(d, function(z)
            is.factor(z) || is.character(z) || is.logical(z), logical(1))]
        chars <- setdiff(chars, tcols)
        if (length(chars)) return(FALSE)
        nms <- names(d)
        idc <- .id_like_cols(d)
        num_non_time <- setdiff(nms[vapply(d, is.numeric, logical(1))], tcols)
        meas <- setdiff(num_non_time, idc)
        length(meas) > max(3L, length(idc) + 1L)
    }
    .hierarchy_cols <- function(d) {
        nms <- names(d)
        if (is.null(nms) || !length(nms)) return(character(0))

        from_fnm <- attr(d, 'f.nm', exact = TRUE)
        from_split <- attr(d, 'split_vars', exact = TRUE)
        facs <- nms[vapply(d, is.factor, logical(1))]
        chars <- nms[vapply(d, function(z) is.character(z) || is.logical(z), logical(1))]
        ids <- .id_like_cols(d)
        fallback <- setdiff(nms, c(.time_cols(d), .measure_cols(d)))

        cand <- unique(c(as.character(from_fnm), as.character(from_split), facs, chars, ids, fallback))
        cand <- intersect(cand, nms)
        cand <- setdiff(cand, .time_cols(d))
        cand[nzchar(cand) & !is.na(cand)]
    }
    .resolve_lv_stage <- function(lv_i, d, stage_i) {
        if (is.null(lv_i)) return(lv_i)
        if (length(lv_i) == 0L) return(lv_i)
        h <- .hierarchy_cols(d)
        nms <- names(d)
        if (is.numeric(lv_i)) {
            idx <- as.integer(lv_i)
            bad <- is.na(idx) | idx < 1L | idx > length(h)
            if (any(bad)) {
                msg <- sprintf("Stage %d: numeric lv %s could not be resolved on current hierarchy [%s].",
                               stage_i, paste(lv_i, collapse = ', '), paste(h, collapse = ', '))
                if (identical(validate, 'stop')) stop(msg, call. = FALSE)
                if (identical(validate, 'warn')) warning(msg, call. = FALSE)
                idx <- idx[!bad]
            }
            if (!length(idx)) return(NULL)
            return(h[idx])
        }
        if (is.character(lv_i)) {
            keep <- intersect(lv_i, h)
            if (!length(keep)) keep <- intersect(lv_i, nms)
            miss <- setdiff(lv_i, keep)
            if (length(miss)) {
                msg <- sprintf("Stage %d: lv names not found in current data [%s].",
                               stage_i, paste(miss, collapse = ', '))
                if (identical(validate, 'stop')) stop(msg, call. = FALSE)
                if (identical(validate, 'warn')) warning(msg, call. = FALSE)
            }
            if (!length(keep)) return(NULL)
            return(keep)
        }
        lv_i
    }

    .fn_name <- function(x) {
        if (is.character(x)) return(as.character(x)[1L])
        if (is.symbol(x)) return(as.character(x))
        if (is.function(x)) {
            nm <- deparse(substitute(x))
            if (length(nm) && nzchar(nm[1L])) return(nm[1L])
        }
        as.character(x)[1L]
    }
    .legacy_terminal_fns <- c('frametoLme', 'frametoFriedman')
    .has_legacy_terminal <- function(x) {
        if (!length(x)) return(FALSE)
        nm <- .fn_name(x[[length(x)]])
        is.character(nm) && length(nm) == 1L && !is.na(nm) && (nm %in% .legacy_terminal_fns)
    }
    .terminal_from_form <- function(form) {
        if (is.null(form)) return(NULL)
        if (inherits(form, 'FriedmanForm')) return('frametoFriedman')
        if (inherits(form, 'formula')) return('frametoLme')
        if (is.character(form) && length(form) == 1L) {
            if (grepl('~', form, fixed = TRUE)) return('frametoLme')
            low <- tolower(form)
            if (low %in% c('tdform','lmeform')) return('frametoLme')
            if (low %in% c('friedmanform')) return('frametoFriedman')
        }
        if (is.list(form) && !is.null(names(form))) {
            if ('response' %in% names(form) || inherits(form, 'FriedmanForm')) {
                return('frametoFriedman')
            }
        }
        NULL
    }

    fn <- .as_stage_list(fn)
    lv <- .as_stage_list(lv)
    legacy_terminal <- if (.has_legacy_terminal(fn)) .fn_name(fn[[length(fn)]]) else NULL
    stage_fn <- if (!is.null(legacy_terminal)) fn[-length(fn)] else fn
    terminal_fun <- .terminal_from_form(form)
    if (is.null(terminal_fun)) terminal_fun <- legacy_terminal
    if (!is.null(legacy_terminal) && !is.null(.terminal_from_form(form)) && !identical(legacy_terminal, .terminal_from_form(form))) {
        .emit(sprintf("Conflicting terminal specifications in 'fn' (%s) and 'form' (%s); using 'form'.",
                      legacy_terminal,
                      if (is.character(form)) form[1L] else class(form)[1L]))
    }

    nstage <- length(stage_fn)
    if (nstage) {
        lv <- .recycle_stage(lv, nstage, 'lv')
    } else {
        lv <- list()
    }

    lse <- list(...)

    if ((is.list(rd) && !is.data.frame(rd)) || .looks_wide(rd)) {
        rd <- shiftFrame(rd)
    }
    if (!is.data.frame(rd)) {
        stop("'rd' must resolve to a data.frame after optional reshaping.", call. = FALSE)
    }

    fns <- 'mUnits'
    munit_args <- intersect(names(lse), names(formals(fns))[-1L])
    if (length(munit_args)) {
        mcols <- .measure_cols(rd)
        if (!length(mcols)) {
            .emit("No record-like numeric columns were detected for mUnits(); skipping unit conversion.")
        } else {
            rdu <- arguSelect(x = rd[, mcols, drop = FALSE], fun = fns, ...)
            rd[, mcols] <- do.call(fns, rdu)
        }
        if ('sc.c' %in% names(lse)) {
            sca <- arguSelect(x = lse[['sc.c']], fun = fns, ...)
            lse[['sc.c']] <- do.call(fns, sca)
        }
    }

    mar <- 'MoreArgs'
    ls_is_list <- vapply(lse, is.list, logical(1))
    yls <- lapply(lse[ls_is_list], unlist, recursive = TRUE, use.names = TRUE)
    yls[['fn']] <- stage_fn
    yls[['lv']] <- lv
    nma <- yls[!names(yls) %in% mar]
    if (nstage) {
        nma <- Map(function(x, nm) .recycle_stage(x, nstage, nm), nma, names(nma))
    } else {
        nma <- nma[0L]
    }
    lsp <- lse[!names(lse) %in% names(yls)]
    if ('MoreArgs' %in% names(lse)) lsp[[mar]] <- lse[[mar]]

    if (nstage) {
        ar <- vector('list', nstage)
        for (i in seq_len(nstage)) {
            lsl <- lapply(nma, '[[', i)
            if ('lv' %in% names(lsl)) {
                lsl[['lv']] <- .resolve_lv_stage(lsl[['lv']], rd, i)
                if (is.null(lsl[['lv']]) || !length(lsl[['lv']])) {
                    stop(sprintf("Stage %d: no valid splitting level could be resolved from 'lv'.", i),
                         call. = FALSE)
                }
            }
            lt <- list(rd = rd, fun = 'ringApply')

            facs <- rd[, vapply(rd, is.factor, logical(1)), drop = FALSE]
            nl <- unique(unlist(lapply(facs, levels), use.names = FALSE))
            is_expandable <- function(x) {
                !is.null(names(x)) && length(names(x)) && any(names(x) %in% nl)
            }
            my <- vapply(lsp, is_expandable, logical(1))
            if (any(my)) {
                lsp[names(lsp)[my]] <- lapply(lsp[names(lsp)[my]], function(x) levexp(x, rd))
            }
            lst <- c(lsl, lsp, lt)
            ar[[i]] <- do.call('arguSelect', lst)
            if ('validate' %in% names(formals(ringApply)) && !('validate' %in% names(ar[[i]]))) {
                ar[[i]][['validate']] <- validate
            }
            rd <- do.call('ringApply', ar[[i]])
            if (!is.data.frame(rd)) {
                stop(sprintf("Stage %d returned a non-data.frame object.", i), call. = FALSE)
            }
        }
    }

    if (is.null(terminal_fun)) {
        .mf_numeric_factorize <- function(z) {
            if (is.factor(z)) {
                lv <- levels(z)
                num <- suppressWarnings(as.numeric(as.character(lv)))
                if (!anyNA(num)) return(factor(as.character(z), levels = as.character(lv[order(num)])))
                return(z)
            }
            if (is.character(z)) {
                num <- suppressWarnings(as.numeric(z))
                if (!anyNA(num[!is.na(z)])) {
                    lv <- sort(unique(num[is.finite(num)]))
                    return(factor(z, levels = as.character(lv)))
                }
                return(factor(z))
            }
            if (is.logical(z)) return(factor(z))
            if (is.numeric(z)) return(factor(z, levels = sort(unique(z))))
            z
        }
        .mf_guess_response <- function(d) {
            xnm <- attr(d, 'x.nm', exact = TRUE)
            if (!is.null(xnm)) {
                hit <- intersect(as.character(xnm), names(d))
                if (length(hit)) return(hit[1L])
            }
            pref <- c('x.idx','x.res','x','rw','trw','ring','ring_width','ringwidth','value','y')
            low <- tolower(names(d))
            hit <- names(d)[match(pref, low, nomatch = 0L)]
            hit <- hit[nzchar(hit)]
            if (length(hit)) return(hit[1L])
            num <- names(d)[vapply(d, is.numeric, logical(1))]
            num <- setdiff(num, c(.time_cols(d), .hierarchy_cols(d)))
            if (length(num)) return(num[1L])
            character(0)
        }
        .mf_make_fluc <- function(d) {
            dat <- as.data.frame(d)
            time_var <- .time_cols(dat)
            time_var <- if (length(time_var)) time_var[1L] else character(0)
            group_vars <- .hierarchy_cols(dat)
            response <- .mf_guess_response(dat)
            if (length(group_vars)) {
                dat[group_vars] <- lapply(dat[group_vars], .mf_numeric_factorize)
            }
            out <- dat
            if (length(response) == 1L && length(time_var) == 1L) {
                form_txt <- if (length(group_vars)) {
                    sprintf('%s ~ %s | %s', response, time_var, paste(group_vars, collapse = '/'))
                } else {
                    sprintf('%s ~ %s', response, time_var)
                }
                out <- tryCatch(nlme::groupedData(as.formula(form_txt), data = dat), error = function(e) dat)
            }
            attr(out, 'mf_response') <- response
            attr(out, 'mf_time') <- time_var
            attr(out, 'mf_groups') <- group_vars
            out
        }
        attr(rd, 'call') <- sys.call()
        attr(rd, 'terminal') <- 'none'
        attr(rd, 'fluc') <- .mf_make_fluc(rd)
        class(rd) <- unique(c('modelFrame', class(rd)))
        return(rd)
    }

    if (identical(terminal_fun, 'frametoLme')) {
        lme_fun <- if (is.character(form) && length(form) == 1L && !grepl('~', form, fixed = TRUE)) form else NULL
        arl <- arguSelect(rd, fun = c('frametoLme','lme', lme_fun), ...)
        arl[['form']] <- form
        if ('validate' %in% names(formals(frametoLme)) && !('validate' %in% names(arl))) {
            arl[['validate']] <- validate
        }
        out <- do.call('frametoLme', arl)
    } else {
        ff_fun <- if (is.character(form) && length(form) == 1L && !grepl('~', form, fixed = TRUE)) form else NULL
        art <- arguSelect(rd, fun = c('frametoFriedman', ff_fun), ...)
        art[['form']] <- form
        if ('validate' %in% names(formals(frametoFriedman)) && !('validate' %in% names(art))) {
            art[['validate']] <- validate
        }
        out <- do.call('frametoFriedman', art)
    }

    if (is.list(out) && !is.null(names(out))) {
        out[['call']] <- sys.call()
        class(out) <- unique(c('modelFrame', class(out)))
        return(out)
    }

    attr(out, 'call') <- sys.call()
    out
### Processed long-format \code{data.frame} if \code{form = NULL};
### otherwise the object returned by the terminal fitter, with the
### originating \code{modelFrame} call attached and class
### \code{\"modelFrame\"} prepended.
} , ex=function() {
    ## ring-width chronologies (mm) of eight cores in two plots, one
    ## column per core
    data(Pchron, envir = environment())
    ## allometric parameters: diameter inside bark (cm) to diameter at
    ## breast height (cm), then to total tree biomass (kg)
    biom_param <- c(2.87, 0.85, 0.05, 2.5)

    ## rtimes, scacum and amod stage by stage, then a model on year with a
    ## random intercept per tree: these 2 plots x 2 trees x 2 cores support
    ## variation among trees only (tree labels are unique across plots here)
    Pchron |>
        modelFrame(to = 'cm', MoreArgs = list(mp = c(2, 1, biom_param)),
                   form = 'log(x) ~ log(csx) + year | tree',
                   random = pdDiag(~ 1))

    ## the same stages with a Friedman smoother as the terminal step
    Pchron |>
        modelFrame(form = 'FriedmanForm', to = 'cm',
                   MoreArgs = list(mp = c(2, 1, biom_param)),
                   response = 'x', time = 'year')

    ## only the processed long data
    Pchron |>
        modelFrame(form = NULL, to = 'cm',
                   MoreArgs = list(mp = c(2, 1, biom_param))) |>
        as.data.frame() |>
        head()
})
