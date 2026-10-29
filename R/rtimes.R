rtimes <- structure(function#Time-units synchronization
### Unique observations in time-series replicates are excluded
                    ## details<<This function is used to enhance
                    ## convergence of mixed-effects parameters during
                    ## detrending processes of multilevel ecological
                    ## data series (see \code{\link{modelFrame}}
                    ## function). For \code{data.frame} inputs, the
                    ## patched version can compute synchronized time
                    ## units either globally or within split blocks
                    ## when \code{lv} is supplied. When \code{lv} is
                    ## not \code{NULL}, the function delegates block
                    ## construction to \code{slitFrame()}, computes the
                    ## time order inside each returned block, and then
                    ## writes the synchronized time values back to the
                    ## original row positions. This preserves the
                    ## original row order while ensuring that time is
                    ## evaluated after splitting and before downstream
                    ## use.
(
    x, ##<< multilevel ecological data series containing a column of
       ##time units, or \code{numeric} vector with names representing
       ##the time units. In a data frame the time units are found by the
       ##package's column names: \code{year} (or \code{hydro_year_id}
       ##from \code{\link{moveYr}}), \code{month} and \code{time}; a
       ##date column is also used. Without any of them, row order is
       ##used and a warning is given.
    only.dup = TRUE, ##<< \code{logical}. Extract only duplicated
                    ##times. If TRUE then unique times are replaced
                    ##with NA. If all computed times are unique then
                    ##this argument is ignored. Rows with NA time, for
                    ##example the first years of a core that no other
                    ##core of its tree covers (16 rows of
                    ##\code{Prings05} with \code{lv = 2} in
                    ##\code{\link{ringApply}}), are later dropped from
                    ##any model that uses \code{time} (see
                    ##\code{\link{frametoLme}}).
    lv = NULL, ##<< optional split level passed to \code{slitFrame()}.
               ## When supplied, synchronized times are computed
               ## within each split block rather than on the whole
               ## table.
    validate = c("none", "warn", "stop") ##<< validation mode.
) {
    validate <- match.arg(validate)
    .signal <- function(msg, severe = FALSE) {
        if (validate == "none") return(invisible(NULL))
        if (severe || validate == "stop") stop(msg, call. = FALSE)
        warning(msg, call. = FALSE)
    }

    .month_num <- function(v) {
        if (inherits(v, "Date") || inherits(v, "POSIXt")) return(NULL)
        if (is.factor(v)) v <- as.character(v)
        if (is.numeric(v) || is.integer(v)) {
            out <- suppressWarnings(as.integer(v))
            if (length(out) && all(is.na(out) | (out >= 1L & out <= 12L))) return(out)
            return(NULL)
        }
        if (is.character(v)) {
            vv <- trimws(v)
            low <- tolower(vv)
            abb <- tolower(month.abb)
            full <- tolower(month.name)
            out <- match(substr(low, 1L, 3L), abb)
            full_match <- match(low, full)
            out[!is.na(full_match)] <- full_match[!is.na(full_match)]
            nums <- suppressWarnings(as.integer(vv))
            ok_num <- !is.na(nums) & nums >= 1L & nums <= 12L
            out[ok_num] <- nums[ok_num]
            if (all(is.na(out))) return(NULL)
            return(out)
        }
        NULL
    }

    .as_numeric_time <- function(v) {
        if (inherits(v, "Date") || inherits(v, "POSIXt")) {
            return(as.numeric(v))
        }
        if (is.factor(v)) v <- as.character(v)
        if (is.logical(v)) return(as.integer(v))
        suppressWarnings(as.numeric(v))
    }

    .detect_time_cols <- function(df) {
        nms <- names(df)
        low <- tolower(nms)
        # date/datetime first
        dcols <- nms[vapply(df, function(z) inherits(z, "Date") || inherits(z, "POSIXt"), logical(1))]
        if (length(dcols)) return(dcols[1L])

        pick_named <- function(patterns) {
            idx <- which(low %in% patterns)
            if (length(idx)) nms[idx[1L]] else character(0)
        }

        ## the package's column contract: 'year' (or 'hydro_year_id' from
        ## moveYr), 'month' and 'time'
        out <- character(0)
        out <- c(out, pick_named(c("hydro_year_id", "year")))
        out <- c(out, pick_named("month"))
        out <- c(out, pick_named("time"))
        unique(out[nzchar(out)])
    }

    .build_time_key <- function(df, time_cols) {
        if (!length(time_cols)) {
            return(list(order = seq_len(nrow(df)), key = as.character(seq_len(nrow(df))), cols = character(0)))
        }

        ord_df <- list()
        used <- character(0)
        for (nm in time_cols) {
            z <- df[[nm]]
            if (inherits(z, "Date") || inherits(z, "POSIXt")) {
                ord_df[[nm]] <- as.numeric(z)
                used <- c(used, nm)
                next
            }
            if (tolower(nm) == "month") {
                mz <- .month_num(z)
                if (!is.null(mz)) {
                    ord_df[[nm]] <- mz
                    used <- c(used, nm)
                    next
                }
            }
            nz <- .as_numeric_time(z)
            if (!all(is.na(nz))) {
                ord_df[[nm]] <- nz
                used <- c(used, nm)
            }
        }

        if (!length(ord_df)) {
            return(list(order = seq_len(nrow(df)), key = as.character(seq_len(nrow(df))), cols = character(0)))
        }

        ord_call <- c(ord_df, list(na.last = TRUE, method = "radix"))
        ord <- do.call(order, ord_call)
        key_df <- as.data.frame(ord_df, optional = TRUE, stringsAsFactors = FALSE)[ord, , drop = FALSE]
        key <- do.call(paste, c(lapply(key_df, function(v) ifelse(is.na(v), "NA", format(v, trim = TRUE, scientific = FALSE))), list(sep = "\r")))
        list(order = ord, key = key, cols = used)
    }

    .time_from_key <- function(key, only_dup = TRUE) {
        first <- !duplicated(key)
        time <- match(key, key[first])
        if (isTRUE(only_dup)) {
            tb <- table(key)
            if (any(tb > 1L)) {
                time[as.integer(tb[key]) == 1L] <- NA_integer_
            }
        }
        as.integer(time)
    }

    .rtimes_vector <- function(v) {
        if (is.null(names(v))) {
            .signal("Unnamed vector supplied to rtimes(); using row order as time labels.", severe = FALSE)
            key <- as.character(seq_along(v))
            ord <- seq_along(v)
            used <- character(0)
        } else {
            nm <- names(v)
            nd <- suppressWarnings(as.numeric(nm))
            if (!all(is.na(nd))) {
                ord <- order(nd, na.last = TRUE, method = "radix")
                key <- format(nd[ord], trim = TRUE, scientific = FALSE)
                used <- "names"
            } else {
                dd <- suppressWarnings(as.numeric(as.Date(nm)))
                if (!all(is.na(dd))) {
                    ord <- order(dd, na.last = TRUE, method = "radix")
                    key <- format(dd[ord], trim = TRUE, scientific = FALSE)
                    used <- "names"
                } else {
                    .signal("Vector names are not numeric/date-like; using row order as time labels.", severe = FALSE)
                    ord <- seq_along(v)
                    key <- as.character(ord)
                    used <- character(0)
                }
            }
        }
        time_ord <- .time_from_key(key, only_dup = only.dup)
        time <- rep.int(NA_integer_, length(v))
        time[ord] <- time_ord
        da <- data.frame(x = v, time = time)
        attr(da, "time_vars") <- used
        da
    }

    if (!is.logical(only.dup) || length(only.dup) != 1L || is.na(only.dup)) {
        stop("'only.dup' must be TRUE or FALSE.", call. = FALSE)
    }

    if (!is.data.frame(x)) {
        return(.rtimes_vector(x))
    }

    cd <- as.data.frame(x, stringsAsFactors = FALSE)
    n <- nrow(cd)
    if (!n) {
        out <- cd
        out$time <- integer(0)
        return(out)
    }

    if (!length(.detect_time_cols(cd))) {
        warning(paste0("rtimes(): none of the columns 'year', 'hydro_year_id', ",
                       "'month' or 'time' (or a date column) was found; ",
                       "row order is used as time."), call. = FALSE)
    }
    cd[[".__rtimes_rowid__"]] <- seq_len(n)
    split_vars <- NULL
    blocks <- NULL
    if (!is.null(lv)) {
        if (!exists("slitFrame", mode = "function")) {
            .signal("'lv' was supplied, but slitFrame() is not available; falling back to global computation.", severe = FALSE)
        } else {
            blocks <- tryCatch(slitFrame(cd, lv), error = function(e) e)
            if (inherits(blocks, "error")) {
                .signal(paste0("slitFrame() failed inside rtimes(): ", conditionMessage(blocks)), severe = (validate == "stop"))
                blocks <- NULL
            } else {
                split_vars <- attr(blocks, "split_vars")
                if (is.null(split_vars)) split_vars <- lv
            }
        }
    }
    if (is.null(blocks)) {
        blocks <- list(global = cd)
        split_vars <- character(0)
    }

    time_out <- rep.int(NA_integer_, n)
    used_time_cols <- character(0)

    for (nm in names(blocks)) {
        blk <- blocks[[nm]]
        rid <- blk[[".__rtimes_rowid__"]]
        blk[[".__rtimes_rowid__"]] <- NULL

        time_cols <- .detect_time_cols(blk)
        if (!length(time_cols)) {
            .signal("No recognizable time columns found in one or more blocks; using row order within block.", severe = FALSE)
        }
        tk <- .build_time_key(blk, time_cols)
        used_time_cols <- union(used_time_cols, tk$cols)
        time_blk <- .time_from_key(tk$key, only_dup = only.dup)
        time_unsorted <- rep.int(NA_integer_, nrow(blk))
        time_unsorted[tk$order] <- time_blk
        time_out[rid] <- time_unsorted
    }

    cd[[".__rtimes_rowid__"]] <- NULL
    if ("time" %in% names(cd)) cd[["time"]] <- time_out else cd[["time"]] <- time_out
    attr(cd, "split_vars") <- split_vars
    attr(cd, "time_vars") <- used_time_cols
    return(cd)
### \code{data.frame} object with the initial vector and its time
### units.
} , ex=function(){
    ## row names of a vector
    fy <- function(y,span){(y - span):y}
    x <- c(fy(2005,5),fy(2007,10))
    ## (not run) Simulating the vector
    r <- abs(rnorm(length(x)))
    names(r) <- x
    ## (not run) computing the synchronized times:
    rtimes(r,only.dup = TRUE)
    ## (not run) Extracting only duplicated times:
    na.omit(rtimes(r,only.dup = TRUE))
})
