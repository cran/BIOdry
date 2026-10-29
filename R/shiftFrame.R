shiftFrame <- structure(function
### dendroclimatic chronologies (trw, and climatic data) are formatted
### into multilevel ecological data series. SI units of continuous
### variables in the data can be transformed.
                        ##details<< Wide BIOdry data may represent either
                        ##tree-ring or climatic series. The patched
                        ##implementation prefers explicit time columns in
                        ##wide data, that is, the column named by
                        ##\code{t.nm}, whole-number numeric columns and the
                        ##\code{month} column, and then treats the remaining value
                        ##columns as hierarchy-encoded series labels. This
                        ##rule applies to both climate and tree-ring wide
                        ##data. Hierarchy levels are parsed from value-column
                        ##names with the configurable regular expression
                        ##given by \code{split_pattern}. The default keeps
                        ##the historical dot-separated BIOdry convention.
                        ##
                        ##When long data are shifted to wide format, time
                        ##variables are retained as independent columns
                        ##rather than being hidden in row names. Value
                        ##columns are then rebuilt from the grouping
                        ##hierarchy using \code{level_sep}. When long
                        ##data contain more than one measured variable,
                        ##for example \code{tm} and \code{pm}, the wide
                        ##columns are prefixed by the measure name, such as
                        ##\code{tm.z1} or \code{pm.z1}, so the reverse
                        ##conversion can recover all value variables. This makes wide
                        ##to long round-trips safer for data with more than
                        ##one time variable, such as \code{year + month} in
                        ##climate series or \code{year + cambial_age} in
                        ##tree-ring series.
                        ##
                        ##Row-name time is still accepted as a backward-
                        ##compatibility fallback for older BIOdry wide
                        ##chronologies. In that case, row names are used only
                        ##when explicit time columns are absent.
                        ##
                        ##When a wide object produced by \code{shiftFrame()}
                        ##is shifted back to long format, the stored metadata
                        ##for \code{x.nm}, \code{f.nm}, \code{t.nm}, and
                        ##\code{time_cols} take precedence over the type rule
                        ##(see \code{\link{cClass}}).
                        ##This makes wide-to-long round-trips deterministic.
                        ##For single-response long results that are returned as
                        ##\code{groupedData}, the grouping formula is built from
                        ##the actual response variable and the stored hierarchy,
                        ##rather than being derived again from column types.
                        ##Long-format outputs are ordered explicitly as record
                        ##or measured variables first, then time variables,
                        ##and finally grouping factors from the lowest
                        ##resolved hierarchy level to the broadest one.
(
    rd,
    f.nm = NULL,
    x.nm = names(rd)[1L],
    t.nm = 'year',
    split_pattern = '\\.',
    level_sep = '.',
    ...
) {
    .msg <- function(type = c('warn','stop'), ...) {
        type <- match.arg(type)
        txt <- paste0(..., collapse = '')
        if (identical(type, 'stop')) stop(txt, call. = FALSE)
        warning(txt, call. = FALSE)
    }

    if (!is.character(split_pattern) || length(split_pattern) != 1L || is.na(split_pattern) || !nzchar(split_pattern)) {
        .msg('stop', "'split_pattern' must be a single non-empty character string.")
    }
    if (!is.character(level_sep) || length(level_sep) != 1L || is.na(level_sep)) {
        .msg('stop', "'level_sep' must be a single character string.")
    }

    .month_num <- function(x) {
        if (is.factor(x)) x <- as.character(x)
        if (inherits(x, c('Date','POSIXct','POSIXt'))) return(NULL)
        if (is.numeric(x)) {
            xi <- suppressWarnings(as.integer(x))
            if (length(xi) && all(is.na(xi) | (xi >= 1L & xi <= 12L))) return(xi)
        }
        if (is.character(x)) {
            xx <- trimws(tolower(x))
            xx <- sub('\\.$', '', xx)
            abb <- tolower(month.abb)
            full <- tolower(month.name)
            out <- rep(NA_integer_, length(xx))
            is_num <- grepl('^[0-9]+$', xx)
            out[is_num] <- suppressWarnings(as.integer(xx[is_num]))
            m1 <- match(substr(xx, 1L, 3L), abb)
            m2 <- match(xx, full)
            out[is.na(out)] <- m1[is.na(out)]
            out[is.na(out)] <- m2[is.na(out)]
            if (all(is.na(out) | (out >= 1L & out <= 12L))) return(out)
        }
        NULL
    }

    ## the package's 'month' column, or a factor whose levels are month
    ## names (see cClass)
    .is_month_col <- function(name, x) {
        .BIOdry_is_month_col(x, name)
    }


    .row_time <- function(rn) {
        if (is.null(rn) || !length(rn)) return(NULL)
        rr <- trimws(rn)
        num <- suppressWarnings(as.numeric(rr))
        if (all(!is.na(num))) return(num)
        rr
    }

    .safe_mUnits <- function(x, ...) {
        if (!exists('mUnits', mode = 'function')) return(x)
        if (exists('arguSelect', mode = 'function')) {
            dtu <- arguSelect(x = x, fun = 'mUnits', ...)
            return(do.call('mUnits', dtu))
        }
        do.call('mUnits', c(list(x = x), list(...)))
    }


    .make_grouped <- function(dt, xvar, tvar, fvars) {
        if (!exists('groupedData', mode = 'function')) return(dt)
        if (!length(xvar) || !nzchar(xvar) || !(xvar %in% names(dt))) return(dt)
        if (!length(tvar) || !nzchar(tvar) || !(tvar %in% names(dt))) return(dt)
        fvars <- fvars[fvars %in% names(dt)]
        if (!length(fvars)) return(dt)
        fr <- as.formula(paste(xvar, '~', tvar, '|', paste(fvars, collapse = '/')))
        gd <- groupedData(formula = fr, data = dt)
        keep <- unique(c(xvar, tvar, fvars, setdiff(names(gd), c(xvar, tvar, fvars))))
        keep <- keep[keep %in% names(gd)]
        gd <- gd[, keep, drop = FALSE]
        attr(gd, 'time_cols') <- attr(dt, 'time_cols')
        attr(gd, 'x.nm') <- attr(dt, 'x.nm')
        attr(gd, 'f.nm') <- attr(dt, 'f.nm')
        attr(gd, 't.nm') <- attr(dt, 't.nm')
        gd
    }

    .time_order_names <- c('hydro_year_id','year','yr','calendar_year','hydro_year',
                           'month','week','day','hour','minute','time','date',
                           'datetime','cambial_age','ring_time','rel_time','age')

    .guess_time_cols <- function(df, preferred = NULL) {
        nms <- names(df)
        out <- character(0)
        if (!is.null(preferred) && length(preferred)) {
            out <- c(out, preferred[preferred %in% nms])
        }
        attr_cols <- attr(df, 'time_cols')
        if (!is.null(attr_cols)) out <- c(out, attr_cols[attr_cols %in% nms])
        ## whole-number columns are time candidates (see cClass), and so is
        ## the package's 'month' column
        out <- c(out, .BIOdry_time_cols(df), intersect('month', nms))
        out <- unique(out)
        ord <- match(tolower(out), .time_order_names)
        out[order(is.na(ord), ord, out)]
    }

    .reorder_month <- function(dt) {
        month_cols <- names(dt)[vapply(seq_along(dt), function(i)
            .is_month_col(names(dt)[i], dt[[i]]), logical(1))]
        if (!length(month_cols)) return(dt)
        mcol <- month_cols[1L]
        mnum <- .month_num(dt[[mcol]])
        dt[[mcol]] <- factor(month.abb[mnum], levels = month.abb)
        grp_cols <- .BIOdry_group_cols(dt)
        ord_cols <- c(setdiff(c(names(dt)[names(dt) == 'year'], mcol), character(0)), mcol)
        ord_cols <- unique(ord_cols[ord_cols %in% names(dt)])
        if (!length(ord_cols)) ord_cols <- mcol
        grp_cols <- setdiff(grp_cols, ord_cols)
        if (length(grp_cols)) {
            sp <- split(dt, interaction(dt[, grp_cols, drop = FALSE], drop = TRUE), drop = TRUE)
            sp <- lapply(sp, function(x) x[do.call(order, c(lapply(x[, ord_cols, drop = FALSE], identity), list(na.last = TRUE))), , drop = FALSE])
            dt <- do.call(rbind, sp)
            rownames(dt) <- NULL
        } else {
            dt <- dt[do.call(order, c(lapply(dt[, ord_cols, drop = FALSE], identity), list(na.last = TRUE))), , drop = FALSE]
            rownames(dt) <- NULL
        }
        dt
    }



    .order_long_cols <- function(dt, time_cols, x_vars, fvars) {
        time_cols <- unique(intersect(time_cols, names(dt)))
        x_vars <- unique(intersect(as.character(x_vars), names(dt)))
        fvars <- unique(intersect(as.character(fvars), names(dt)))
        other <- setdiff(names(dt), c(time_cols, x_vars, fvars))
        dt[, c(x_vars, time_cols, fvars, other), drop = FALSE]
    }

    wide2long <- function(rd, f.nm, ...) {
        sepl <- split_pattern
        nms <- names(rd)
        if (!length(nms)) .msg('stop', 'Wide chronology data must have column names.')

        # Prefer explicit time columns in wide data
        time_cols <- .guess_time_cols(rd, preferred = t.nm)
        time_cols <- intersect(time_cols, names(rd))
        value_cols <- setdiff(names(rd), time_cols)

        if (length(time_cols) && length(value_cols)) {
            if (is.null(f.nm)) f.nm <- attr(rd, 'f.nm')
            xvr <- attr(rd, 'x.nm')
            if (is.null(xvr)) xvr <- x.nm
            xvr <- as.character(xvr)
            if (!length(xvr)) xvr <- 'x'

            parts <- strsplit(value_cols, split = sepl, perl = TRUE)
            raw_nparts <- vapply(parts, length, integer(1))
            if (!length(raw_nparts)) .msg('stop', 'No value columns were found after removing explicit time columns.')

            measure <- NULL
            group_parts <- parts
            if (length(xvr) > 1L) {
                firsts <- vapply(parts, function(z) if (length(z)) z[1L] else '', character(1))
                ok <- firsts %in% xvr & raw_nparts >= 2L
                if (all(ok)) {
                    measure <- firsts
                    group_parts <- lapply(parts, function(z) z[-1L])
                }
            }

            nparts <- vapply(group_parts, length, integer(1))
            if (length(unique(nparts)) != 1L) {
                if (is.null(f.nm) && all(nparts == 1L)) {
                    f.nm <- 'F1'
                } else {
                    .msg('stop', 'Wide value-column names must have a consistent number of hierarchy levels under split_pattern.')
                }
            }
            if (all(nparts == 1L) && is.null(f.nm)) f.nm <- 'F1'
            lev.1 <- as.data.frame(do.call(rbind, lapply(group_parts, function(z) if (length(z) == 1L) z else z)),
                                   stringsAsFactors = TRUE, check.names = FALSE)
            if (is.null(f.nm)) f.nm <- paste('F', seq_len(ncol(lev.1)), sep = '')
            if (length(f.nm) != ncol(lev.1)) {
                .msg('stop', 'Length of f.nm must match the number of hierarchy levels parsed from wide value-column names.')
            }
            names(lev.1) <- f.nm
            lev.1 <- lev.1[, rev(names(lev.1)), drop = FALSE]
            time_df <- rd[, time_cols, drop = FALSE]

            key_cols <- c(time_cols, names(lev.1))
            blocks <- vector('list', length(value_cols))
            for (i in seq_along(value_cols)) {
                nm_meas <- if (!is.null(measure)) measure[i] else xvr[1L]
                d <- cbind(time_df, lev.1[rep(i, nrow(time_df)), , drop = FALSE], stringsAsFactors = TRUE)
                d[[nm_meas]] <- rd[[value_cols[i]]]
                d <- d[!is.na(d[[nm_meas]]), , drop = FALSE]
                d[[nm_meas]] <- .safe_mUnits(d[[nm_meas]], ...)
                blocks[[i]] <- d
            }

            if (!is.null(measure) && length(unique(measure)) > 1L) {
                meas_levels <- unique(measure)
                meas_blocks <- lapply(meas_levels, function(mm) {
                    idx <- which(measure == mm)
                    dd <- do.call(rbind, blocks[idx])
                    rownames(dd) <- NULL
                    dd
                })
                names(meas_blocks) <- meas_levels
                dt <- Reduce(function(a, b) merge(a, b, by = key_cols, all = TRUE, sort = FALSE), meas_blocks)
            } else {
                dt <- do.call(rbind, blocks)
                rownames(dt) <- NULL
            }
            dt <- .reorder_month(dt)
            x_out <- if (!is.null(measure)) sort(unique(measure)) else xvr[1L]
            dt <- .order_long_cols(dt, time_cols = time_cols, x_vars = x_out, fvars = f.nm)
            attr(dt, 'time_cols') <- time_cols
            attr(dt, 'x.nm') <- x_out
            attr(dt, 'f.nm') <- f.nm
            attr(dt, 't.nm') <- if (length(time_cols)) time_cols[1L] else t.nm
            if (!is.null(measure) && length(unique(measure)) > 1L) return(dt)
            return(.make_grouped(dt, xvar = attr(dt, 'x.nm')[1L], tvar = attr(dt, 't.nm'), fvars = attr(dt, 'f.nm')))
        }

        # Backward-compatible row-name time fallback
        if (is.null(f.nm)) f.nm <- attr(rd, 'f.nm')
        parts <- strsplit(nms, split = sepl, perl = TRUE)
        nparts <- vapply(parts, length, integer(1))
        if (length(unique(nparts)) != 1L) {
            .msg('stop', 'Wide chronology column names must have a consistent number of hierarchy levels under split_pattern.')
        }
        if (!is.null(f.nm) && length(f.nm) == 1L && unique(nparts) == 1L) {
            parts <- lapply(nms, function(z) z)
            nparts <- rep.int(1L, length(nms))
        }
        lev.1 <- as.data.frame(do.call(rbind, parts), stringsAsFactors = TRUE)
        if (is.null(f.nm)) f.nm <- paste('F', seq_len(ncol(lev.1)), sep = '')
        if (length(f.nm) != ncol(lev.1)) {
            .msg('stop', 'Length of f.nm must match the number of hierarchy levels parsed from wide column names.')
        }
        names(lev.1) <- rev(f.nm)
        lev.1 <- lev.1[, f.nm, drop = FALSE]

        rk <- rownames(rd)
        if (is.null(rk) || !length(rk)) .msg('stop', 'Wide chronology row names must encode time units when explicit time columns are absent.')
        yr.nm <- attr(rd, 't.nm')
        if (is.null(yr.nm)) yr.nm <- t.nm
        xvr <- attr(rd, 'x.nm')
        if (is.null(xvr)) xvr <- x.nm
        rtime <- .row_time(rk)
        if (is.null(rtime)) .msg('stop', 'Wide chronology row names must encode time units when explicit time columns are absent.')
        time_df <- data.frame(tmp_time = rtime, check.names = FALSE)
        names(time_df) <- yr.nm
        x <- unlist(as.list(rd), use.names = FALSE)
        time_rep <- time_df[rep(seq_len(nrow(time_df)), ncol(rd)), , drop = FALSE]
        dt <- data.frame(x = x, time_rep, lev.1[rep(seq_len(nrow(lev.1)), each = nrow(time_df)), , drop = FALSE],
                         stringsAsFactors = TRUE, check.names = FALSE)
        names(dt)[1L] <- xvr[1L]
        dt <- dt[!is.na(dt[[xvr[1L]]]), , drop = FALSE]
        dt[[xvr[1L]]] <- .safe_mUnits(dt[[xvr[1L]]], ...)
        dt <- .reorder_month(dt)
        dt <- .order_long_cols(dt, time_cols = names(time_df), x_vars = xvr[1L], fvars = f.nm)
        attr(dt, 'time_cols') <- names(time_df)
        attr(dt, 'x.nm') <- xvr[1L]
        attr(dt, 'f.nm') <- f.nm
        attr(dt, 't.nm') <- yr.nm
        .make_grouped(dt, xvar = attr(dt, 'x.nm')[1L], tvar = attr(dt, 't.nm'), fvars = attr(dt, 'f.nm'))
    }

    long2wide <- function(rd, x.nm, t.nm, ...) {
        time_cols <- .guess_time_cols(rd, preferred = t.nm)
        if (!length(time_cols)) .msg('stop', 'Time variable ', sQuote(t.nm), ' was not found in long-format data.')
        time_cols <- unique(c(intersect(c(t.nm), names(rd)), setdiff(time_cols, t.nm)))
        time_cols <- time_cols[time_cols %in% names(rd)]

        grp_cols <- setdiff(.BIOdry_group_cols(rd), time_cols)

        xvr_attr <- attr(rd, 'x.nm')
        x_candidates <- unique(as.character(c(if (!is.null(xvr_attr)) xvr_attr else character(0), x.nm)))
        num_non_group <- names(rd)[vapply(seq_along(rd), function(i) {
            nm <- names(rd)[i]
            z <- rd[[i]]
            (is.numeric(z) || is.integer(z)) && !(nm %in% c(time_cols, grp_cols))
        }, logical(1))]
        x_vars <- unique(c(intersect(x_candidates, names(rd)), setdiff(num_non_group, x_candidates)))
        if (!length(x_vars)) .msg('stop', 'No value variables were found in long-format data.')
        x_vars <- x_vars[x_vars %in% names(rd)]
        for (nm in x_vars) rd[[nm]] <- .safe_mUnits(rd[[nm]], ...)

        ord_cols <- time_cols[time_cols %in% names(rd)]
        if (length(ord_cols)) {
            rd <- rd[do.call(order, c(lapply(rd[, ord_cols, drop = FALSE], identity), list(na.last = TRUE))), , drop = FALSE]
            rownames(rd) <- NULL
        }

        if (!length(grp_cols)) {
            out <- rd[, c(time_cols, x_vars), drop = FALSE]
            attr(out, 'f.nm') <- character(0)
            attr(out, 'x.nm') <- x_vars
            attr(out, 't.nm') <- t.nm
            attr(out, 'time_cols') <- time_cols
            return(out)
        }

        sp <- split(rd, interaction(rd[, grp_cols, drop = FALSE], drop = TRUE), drop = TRUE)
        blocks <- list()
        bi <- 1L
        for (g in seq_along(sp)) {
            d <- sp[[g]]
            if (length(ord_cols)) {
                d <- d[do.call(order, c(lapply(d[, ord_cols, drop = FALSE], identity), list(na.last = TRUE))), , drop = FALSE]
            }
            key <- do.call(paste, c(d[, time_cols, drop = FALSE], sep = '\r'))
            if (anyDuplicated(key)) {
                .msg('warn', 'Repeated time values were found within at least one group; keeping the first record per time key during wide reshaping.')
                keep <- !duplicated(key)
                d <- d[keep, , drop = FALSE]
                key <- key[keep]
            }
            group_label <- paste(vapply(d[1, grp_cols, drop = FALSE], as.character, character(1)), collapse = level_sep)
            for (xv in x_vars) {
                col_nm <- if (length(x_vars) > 1L) paste(c(xv, group_label), collapse = level_sep) else group_label
                out <- data.frame(.merge_key = key, check.names = FALSE)
                out[[col_nm]] <- d[[xv]]
                attr(out, 'time_df') <- d[, time_cols, drop = FALSE]
                blocks[[bi]] <- out
                bi <- bi + 1L
            }
        }
        wide_vals <- Reduce(function(x, y) merge(x, y, by = '.merge_key', all = TRUE, sort = FALSE), blocks)
        first_time_df <- attr(blocks[[1L]], 'time_df')
        if (is.null(first_time_df) || !is.data.frame(first_time_df)) {
            .msg('stop', 'Internal time metadata were lost during long-to-wide reshaping.')
        }
        first_time <- data.frame(.merge_key = blocks[[1L]][['.merge_key']], first_time_df, check.names = FALSE)
        wide <- merge(first_time, wide_vals, by = '.merge_key', all = TRUE, sort = FALSE)
        series_cols <- setdiff(names(wide), c('.merge_key', time_cols))
        series_cols <- sort(series_cols)
        out <- wide[, c(time_cols[time_cols %in% names(wide)], series_cols[series_cols %in% names(wide)]), drop = FALSE]
        attr(out, 'f.nm') <- grp_cols
        attr(out, 'x.nm') <- x_vars
        attr(out, 't.nm') <- t.nm
        attr(out, 'time_cols') <- time_cols
        out
    }

    if (is.data.frame(rd)) {
        stored_t <- attr(rd, 't.nm')
        stored_x <- attr(rd, 'x.nm')
        stored_f <- attr(rd, 'f.nm')
        stored_tc <- attr(rd, 'time_cols')

        explicit_time_cols0 <- .guess_time_cols(rd, preferred = c(stored_tc, t.nm))
        explicit_time_cols0 <- intersect(explicit_time_cols0, names(rd))

        t.nm.use <- if (!is.null(stored_t) && (t.nm %in% explicit_time_cols0 || !t.nm %in% names(rd))) stored_t else t.nm
        x.nm.use <- if (!is.null(stored_x) && (x.nm %in% explicit_time_cols0 || !x.nm %in% names(rd))) stored_x else x.nm
        f.nm.use <- if (is.null(f.nm) && !is.null(stored_f)) stored_f else f.nm

        explicit_time_cols <- .guess_time_cols(rd, preferred = c(stored_tc, t.nm.use))
        explicit_time_cols <- intersect(explicit_time_cols, names(rd))
        non_time_cols <- setdiff(names(rd), explicit_time_cols)
        value_like_cols <- non_time_cols[vapply(non_time_cols, function(nm) {
            z <- rd[[nm]]
            is.numeric(z) || is.integer(z)
        }, logical(1))]
        wide_like <- FALSE
        # If a previous shiftFrame() call stored the data contract, trust it.
        if (length(explicit_time_cols) && length(non_time_cols) && !is.null(stored_x) && !is.null(stored_tc)) {
            wide_like <- TRUE
        } else if (length(explicit_time_cols) && length(value_like_cols) && (!x.nm.use %in% names(rd) || !is.null(stored_x) || !is.null(stored_f))) {
            # wide data with explicit time columns and value columns
            wide_like <- TRUE
        } else {
            row_time <- .row_time(rownames(rd))
            all_num_non_time <- length(non_time_cols) && all(vapply(rd[, non_time_cols, drop = FALSE], is.numeric, logical(1)))
            col_split <- length(non_time_cols) && all(grepl(split_pattern, non_time_cols, perl = TRUE))
            if (!length(explicit_time_cols) && !is.null(row_time) && (all_num_non_time || col_split || !is.null(stored_f))) {
                wide_like <- TRUE
            }
        }

        if (wide_like) {
            dt <- wide2long(rd, f.nm = f.nm.use, ...)
        } else {
            dt <- long2wide(rd, x.nm = x.nm.use, t.nm = t.nm.use, ...)
        }
    } else if (is.list(rd)) {
        rdl <- Map(function(x) {
            if (!is.data.frame(x)) .msg('stop', 'List elements supplied to shiftFrame() must be data frames.')
            wide2long(x, f.nm, ...)
        }, rd)
        id_cols <- cClass(rdl[[1L]], c('integer', 'factor', 'character'))
        merdt <- Reduce(function(x, y) merge(x, y, by = id_cols, all = TRUE, sort = FALSE), rdl)
        dt <- .reorder_month(merdt)
        x_keep <- intersect(attr(dt, 'x.nm'), names(dt))
        f_keep <- intersect(attr(dt, 'f.nm'), names(dt))
        tc_keep <- intersect(attr(dt, 'time_cols'), names(dt))
        if (length(x_keep) || length(f_keep) || length(tc_keep)) dt <- .order_long_cols(dt, tc_keep, x_keep, f_keep)
    } else {
        .msg('stop', 'rd must be a data.frame or a list of data.frames.')
    }

    dt
}, ex = function(){
    data(Prings05, envir = environment())
    pwide <- shiftFrame(Prings05, from = 'mm', to = 'mmm')
    str(pwide)
    plong <- shiftFrame(pwide, from = 'mmm', to = 'mm')
    plot(plong)
})
