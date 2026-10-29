slitFrame <- structure(function
### Robust multilevel splitting
(
    rd, ##<< data.frame with grouping columns.
    lv = NULL, ##<< NULL, character, or numeric selector for splitting columns.
    validate = c("none", "warn", "stop") ##<< validation mode.
) {
    validate <- match.arg(validate)
    signal_issue <- function(msg) {
        if (validate %in% "stop") stop(msg, call. = FALSE)
        if (validate %in% "warn") warning(msg, call. = FALSE)
        invisible(NULL)
    }
    if (!is.data.frame(rd)) {
        stop("'rd' must be a data.frame.", call. = FALSE)
    }
    if (!nrow(rd)) {
        signal_issue("'rd' has zero rows; returning an empty list.")
        return(list())
    }
    nms <- names(rd)
    if (is.null(nms) || !length(nms)) {
        stop("'rd' must have column names.", call. = FALSE)
    }

    detect_grouping_columns <- function(x) {
        # factor and character columns that are not months (see cClass)
        .BIOdry_group_cols(x)
    }

    default_lv <- detect_grouping_columns(rd)

    if (is.null(lv)) {
        lv <- default_lv
        if (!length(lv)) {
            signal_issue("No grouping columns detected; using all factor columns if available.")
            lv <- names(rd)[vapply(rd, is.factor, logical(1))]
        }
    } else if (is.numeric(lv)) {
        idx <- unique(as.integer(lv[is.finite(lv)]))
        if (!length(idx)) {
            stop("'lv' does not contain valid numeric indices.", call. = FALSE)
        }
        idx <- idx[idx >= 1L]
        if (!length(idx)) {
            stop("'lv' indices must be >= 1.", call. = FALSE)
        }
        if (length(default_lv) && max(idx) <= length(default_lv)) {
            lv <- default_lv[idx]
        } else {
            if (max(idx) > ncol(rd)) {
                stop("Numeric 'lv' indices are out of bounds for detected grouping columns and for 'rd'.",
                     call. = FALSE)
            }
            lv <- nms[idx]
            signal_issue("Interpreting numeric 'lv' as absolute column positions in 'rd'.")
        }
    } else if (is.character(lv)) {
        lv <- unique(lv[nzchar(lv)])
        miss <- setdiff(lv, nms)
        if (length(miss)) {
            stop(
                paste0("Unknown splitting columns: ", paste(miss, collapse = ", ")),
                call. = FALSE
            )
        }
    } else {
        stop("'lv' must be NULL, numeric, or character.", call. = FALSE)
    }

    lv <- unique(lv)
    if (!length(lv)) {
        stop("No splitting columns were resolved.", call. = FALSE)
    }

    split_df <- rd[, lv, drop = FALSE]
    bad_cols <- names(split_df)[vapply(split_df, is.list, logical(1))]
    if (length(bad_cols)) {
        stop(
            paste0("Cannot split on list columns: ", paste(bad_cols, collapse = ", ")),
            call. = FALSE
        )
    }

    norm_for_split <- function(x) {
        if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
            return(factor(as.character(x), exclude = NULL))
        }
        if (is.factor(x)) {
            return(factor(as.character(x), exclude = NULL))
        }
        if (is.character(x) || is.logical(x)) {
            return(factor(as.character(x), exclude = NULL))
        }
        if (is.numeric(x) || is.integer(x)) {
            return(factor(as.character(x), exclude = NULL))
        }
        factor(as.character(x), exclude = NULL)
    }

    split_key <- as.data.frame(lapply(split_df, norm_for_split), stringsAsFactors = FALSE)
    idx <- interaction(split_key, drop = TRUE, lex.order = TRUE, sep = ".")
    lrd <- split(rd, idx, drop = TRUE)

    nm <- names(lrd)
    suppressWarnings(num_nm <- as.numeric(nm))
    if (length(nm) && all(!is.na(num_nm))) {
        lrd <- lrd[order(num_nm)]
    } else if (length(nm)) {
        lrd <- lrd[order(nm)]
    }

    attr(lrd, "split_vars") <- lv
    attr(lrd, "split_levels") <- split_df
    lrd
### list of data.frames.
},
ex = function() {
    data(Prings05, envir = environment())
    data(PTclim05, envir = environment())
    spl <- slitFrame(Prings05)
    str(spl)
    spl <- slitFrame(Prings05, "year")
    str(spl)
    spl <- slitFrame(PTclim05, c("plot", "year"))
    str(spl)
})
