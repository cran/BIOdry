scacum <- structure(function
### This function computes cumulative and optionally scaled sums of ordered
### time-series replicates.
                    ##details<< Cumulative sums of time-series replicates
                    ## (e.g. tree-ring widths) can be computed on a single
                    ## ordered block, or within multilevel data frames after
                    ## splitting by a specified level. For data frames,
                    ## \code{scacum()} orders each block on available time
                    ## variables before computing cumulative sums and returns
                    ## the original data with an added \code{csx} column.
                    ## The value column is \code{x}, the package's column
                    ## name; a response declared with
                    ## \code{\link{defineDataRoles}} takes precedence.
                    ## Without either, \code{scacum()} stops.
                    ## The time variables are found by the package's
                    ## column names: \code{year} (or \code{hydro_year_id}
                    ## from \code{\link{moveYr}}), \code{month} and
                    ## \code{time}; \code{rf.t} refers to the first of
                    ## \code{hydro_year_id}, \code{year} and \code{time}
                    ## that is present.
(
    x, ##<< Numeric vector of time-series replicates, or a data frame.
    sc.c = NA, ##<< Numeric scaling constant. If \code{NA}, cumulative sums
               ## are not scaled.
    rf.t = NA, ##<< Reference time for scaling. If \code{NA}, the last time
               ## value of each ordered block is used. If given, the data
               ## must have a \code{year}, \code{hydro_year_id} or
               ## \code{time} column; otherwise \code{scacum()} stops.
    lv = NULL, ##<< Optional split level for multilevel data frames. Can be
               ## \code{NULL}, character, or numeric, and is resolved through
               ## \code{slitFrame()}.
    validate = c('none', 'warn', 'stop') ##<< Validation mode.
) {
    validate <- match.arg(validate)

    .signal <- function(msg, kind = c('warn', 'stop')) {
        kind <- match.arg(kind)
        if (validate == 'none') return(invisible(FALSE))
        if (validate == 'warn' || kind == 'warn') {
            warning(msg, call. = FALSE)
            return(invisible(TRUE))
        }
        stop(msg, call. = FALSE)
    }

    .month_index <- function(v) {
        if (is.factor(v)) v <- as.character(v)
        if (is.numeric(v) || is.integer(v)) {
            out <- suppressWarnings(as.integer(v))
            ok <- !is.na(out) & out >= 1L & out <= 12L
            if (all(ok | is.na(out))) return(out)
            return(rep(NA_integer_, length(v)))
        }
        if (is.character(v)) {
            vv <- trimws(tolower(v))
            abb <- tolower(month.abb)
            full <- tolower(month.name)
            out <- match(substr(vv, 1L, 3L), abb)
            miss <- is.na(out)
            if (any(miss)) out[miss] <- match(vv[miss], full)
            return(out)
        }
        rep(NA_integer_, length(v))
    }

    .order_keys <- function(df) {
        nms <- names(df)
        keys <- list()
        add <- function(name, vec) {
            if (!is.null(vec) && length(vec) == nrow(df)) keys[[name]] <<- vec
        }

        if ('hydro_year_id' %in% nms) add('hydro_year_id', suppressWarnings(as.numeric(df[['hydro_year_id']])))
        if ('year' %in% nms)          add('year', suppressWarnings(as.numeric(df[['year']])))
        if ('month' %in% nms)         add('month', .month_index(df[['month']]))
        if ('time' %in% nms)          add('time', suppressWarnings(as.numeric(df[['time']])))

        keys <- Filter(function(z) !all(is.na(z)), keys)
        keys
    }

    .primary_time_labels <- function(df) {
        nms <- names(df)
        cand <- c('hydro_year_id', 'year', 'time')
        hit <- cand[cand %in% nms][1L]
        if (!is.na(hit) && length(hit)) {
            return(as.character(df[[hit]]))
        }
        if ('month' %in% nms) {
            mi <- .month_index(df[['month']])
            if (!all(is.na(mi))) return(as.character(mi))
        }
        as.character(seq_len(nrow(df)))
    }

    .resolve_rf <- function(labels, rf.t) {
        labs <- as.character(labels)
        if (!length(labs)) return(list(pos = NA_integer_, label = NA_character_))
        if (length(rf.t) == 1L && is.na(rf.t)) {
            return(list(pos = length(labs), label = labs[length(labs)]))
        }
        rf_chr <- as.character(rf.t[1L])
        pos <- match(rf_chr, labs)
        if (!is.na(pos)) return(list(pos = pos, label = labs[pos]))

        rf_num <- suppressWarnings(as.numeric(rf.t[1L]))
        labs_num <- suppressWarnings(as.numeric(labs))
        if (!is.na(rf_num) && !all(is.na(labs_num))) {
            pos <- match(rf_num, labs_num)
            if (!is.na(pos)) return(list(pos = pos, label = labs[pos]))
        }
        if (!is.na(rf_num) && rf_num >= 1L && rf_num <= length(labs) && rf_num == floor(rf_num)) {
            pos <- as.integer(rf_num)
            return(list(pos = pos, label = labs[pos]))
        }
        .signal("Reference time 'rf.t' was not found in one or more blocks; using the last time value instead.", 'warn')
        list(pos = length(labs), label = labs[length(labs)])
    }

    ## the value column is 'x' (the package's column contract); a response
    ## declared with defineDataRoles() takes precedence
    .value_column <- function(df) {
        nms <- names(df)
        meta <- attr(df, 'BIOdry_meta', exact = TRUE)
        if (.BIOdry_declared(meta, 'response')) {
            decl <- intersect(as.character(c(meta$active_response, meta$response)), nms)
            if (length(decl)) return(decl[1L])
        }
        if ('x' %in% nms) return('x')
        stop(paste0("scacum(): the data have no column 'x' (the package's ",
                    "value column), and no response was declared with ",
                    "defineDataRoles()."), call. = FALSE)
    }

    .process_block <- function(df, x_col, sc.c, rf.t) {
        keys <- .order_keys(df)
        ord <- if (length(keys)) {
            ord_args <- c(keys, list(na.last = TRUE, method = 'radix'))
            do.call(order, ord_args)
        } else {
            seq_len(nrow(df))
        }
        dfo <- df[ord, , drop = FALSE]
        vals <- dfo[[x_col]]
        if (!is.numeric(vals)) vals <- suppressWarnings(as.numeric(vals))
        if (all(is.na(vals))) {
            .signal("All values in the selected series column are NA within one or more blocks.", 'warn')
        }
        xcum <- cumsum(vals)
        labs <- .primary_time_labels(dfo)
        ref <- .resolve_rf(labs, rf.t)
        csx <- xcum
        if (!(length(sc.c) == 1L && is.na(sc.c))) {
            inc <- sc.c[1L] - xcum[ref$pos]
            csx <- xcum + inc
            if (any(csx < 0, na.rm = TRUE)) {
                .signal("Scaled cumulative sums became negative in one or more blocks; falling back to unscaled cumulative sums for those blocks.", 'warn')
                csx <- xcum
            }
        }
        data.frame(.rowid__ = dfo[['.rowid__']], csx = csx, stringsAsFactors = FALSE)
    }

    if (!is.data.frame(x)) {
        if (is.null(names(x))) stop('NULL labels in x', call. = FALSE)
        vals <- x
        if (!is.numeric(vals)) vals <- suppressWarnings(as.numeric(vals))
        xcum <- cumsum(vals)
        labs <- as.character(names(x))
        ref <- .resolve_rf(labs, rf.t)
        csx <- xcum
        if (!(length(sc.c) == 1L && is.na(sc.c))) {
            inc <- sc.c[1L] - xcum[ref$pos]
            csx <- xcum + inc
            if (any(csx < 0, na.rm = TRUE)) {
                .signal("Scaled cumulative sums became negative; falling back to unscaled cumulative sums.", 'warn')
                csx <- xcum
            }
        }
        xd <- data.frame(x = vals, csx = csx)
        row.names(xd) <- names(x)
        return(xd)
    }

    df <- x
    if (!nrow(df)) {
        out <- df
        out[['csx']] <- numeric(0)
        return(out)
    }
    ## rf.t refers to a year or time column; without one there is nothing
    ## to refer to
    if (!(length(rf.t) == 1L && is.na(rf.t)) &&
        !any(c('hydro_year_id', 'year', 'time') %in% names(df))) {
        stop(paste0("scacum(): 'rf.t' was given, but the data have no ",
                    "'year', 'hydro_year_id' or 'time' column for it to refer to."),
             call. = FALSE)
    }
    df[['.rowid__']] <- seq_len(nrow(df))
    x_col <- .value_column(df)

    blocks <- NULL
    split_vars <- character(0)
    if (is.null(lv)) {
        blocks <- list(`1` = df)
    } else {
        if (!exists('slitFrame', mode = 'function')) {
            stop("scacum() with 'lv' requires slitFrame() to be available.", call. = FALSE)
        }
        blocks <- slitFrame(df, lv)
        split_vars <- attr(blocks, 'split_vars')
        if (is.null(split_vars)) split_vars <- character(0)
    }

    res_list <- lapply(blocks, .process_block, x_col = x_col, sc.c = sc.c, rf.t = rf.t)
    res <- do.call(rbind, res_list)
    res <- res[order(res[['.rowid__']]), , drop = FALSE]

    out <- df[order(df[['.rowid__']]), setdiff(names(df), '.rowid__'), drop = FALSE]
    if ('csx' %in% names(out)) out[['csx']] <- NULL
    out[['csx']] <- res[['csx']]
    row.names(out) <- row.names(x)
    attr(out, 'split_vars') <- split_vars
    attr(out, 'time_vars') <- names(.order_keys(df))
    out
}, ex = function() {
    x <- c(0.79,0.32,0.53,0.43,0.18)
    names(x) <- 1948:1952
    scacum(x, sc.c = 4, rf.t = 1951)

    max(cumsum(x))
    scacum(x, NA, 1951)
})
