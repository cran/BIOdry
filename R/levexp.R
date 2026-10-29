levexp <- structure(function
### Fast and compatibility-oriented vector releveling
### Expands or reduces a named numeric vector by matching its level names
### to split labels from multilevel ecological data.
(
    x, ##<< named numeric vector with level names to be matched.
    levels, ##<< data.frame, split list, or character vector of split names.
    validate = c("none", "warn", "stop") ##<< validation mode.
) {
    validate <- match.arg(validate)
    signal_issue <- function(msg) {
        if (validate %in% "stop") stop(msg, call. = FALSE)
        if (validate %in% "warn") warning(msg, call. = FALSE)
        invisible(NULL)
    }

    if (is.null(x) || !length(x)) {
        signal_issue("'x' is empty; returning an empty vector.")
        return(x)
    }
    if (is.null(names(x)) || !length(names(x)) || any(!nzchar(names(x)))) {
        stop("'x' must be a named vector.", call. = FALSE)
    }

    # preserve old split-like grouping of repeated names in x
    x_names <- names(x)
    ux <- unique(x_names)
    tx <- lapply(ux, function(nm) x[x_names %in% nm])
    names(tx) <- ux

    detect_grouping_columns <- function(df) {
        # factor and character columns that are not months (see cClass)
        .BIOdry_group_cols(df)
    }
    norm_for_split <- function(z) {
        if (inherits(z, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
            return(factor(as.character(z), exclude = NULL))
        }
        if (is.factor(z)) return(factor(as.character(z), exclude = NULL))
        if (is.character(z) || is.logical(z)) return(factor(as.character(z), exclude = NULL))
        if (is.numeric(z) || is.integer(z)) return(factor(as.character(z), exclude = NULL))
        factor(as.character(z), exclude = NULL)
    }

    resolve_split_names <- function(levels) {
        # already split list or named object
        if (is.list(levels) && !is.data.frame(levels)) {
            nm <- names(levels)
            if (is.null(nm) || !length(nm)) {
                stop("When 'levels' is a list, it must have names.", call. = FALSE)
            }
            return(nm)
        }

        # character vector of split names / levels
        if (is.character(levels)) {
            if (!length(levels)) return(character(0))
            # approximate old split(levels, levels) naming behavior
            return(sort(unique(levels)))
        }

        if (!is.data.frame(levels)) {
            stop("'levels' must be a data.frame, named list, or character vector.",
                 call. = FALSE)
        }
        if (!nrow(levels)) return(character(0))

        lv <- attr(levels, "split_vars")
        if (is.null(lv) || !length(lv)) {
            lv <- detect_grouping_columns(levels)
            if (!length(lv)) {
                lv <- names(levels)[vapply(levels, is.factor, logical(1))]
            }
        }
        if (!length(lv)) {
            stop("No grouping columns could be resolved from 'levels'.", call. = FALSE)
        }
        miss <- setdiff(lv, names(levels))
        if (length(miss)) {
            stop(paste0("Grouping columns not found in 'levels': ",
                        paste(miss, collapse = ", ")),
                 call. = FALSE)
        }

        split_df <- as.data.frame(lapply(levels[, lv, drop = FALSE], norm_for_split),
                                  stringsAsFactors = FALSE)
        idx <- interaction(split_df, drop = TRUE, lex.order = TRUE, sep = ".")
        nm <- levels(idx)
        suppressWarnings(num_nm <- as.numeric(nm))
        if (length(nm) && all(!is.na(num_nm))) {
            nm <- nm[order(num_nm)]
        } else if (length(nm)) {
            nm <- nm[order(nm)]
        }
        nm
    }

    split_names <- resolve_split_names(levels)
    if (!length(split_names)) return(x[FALSE])

    # pre-tokenize split names once; exact token matching is safer than regex
    split_tokens <- strsplit(split_names, "\\.")
    names(split_tokens) <- split_names

    result <- rep(NA_real_, length(split_names))
    names(result) <- split_names
    unmatched <- character(0)

    for (nm in names(tx)) {
        val <- tx[[nm]]
        if (!length(val)) next
        q <- as.character(nm)
        # a name matches a split name when its dot-separated tokens are a
        # contiguous run of the split name's tokens, read in either direction:
        # names in x go from outer to inner level (e.g. "P16106.17"), split
        # names follow the column order of the data (e.g. "a.17.P16106")
        q_tokens <- strsplit(q, ".", fixed = TRUE)[[1L]]
        has_run <- function(tok) {
            n <- length(q_tokens)
            if (!n || n > length(tok)) return(FALSE)
            any(vapply(seq_len(length(tok) - n + 1L),
                       function(k) identical(tok[k:(k + n - 1L)], q_tokens),
                       logical(1)))
        }
        hit <- vapply(
            split_names,
            function(sn) identical(sn, q) || has_run(split_tokens[[sn]]) ||
                has_run(rev(split_tokens[[sn]])),
            logical(1)
        )
        if (!any(hit)) {
            unmatched <- c(unmatched, q)
            next
        }
        # compatibility: if repeated entries exist for the same name, recycle them
        # over all matches; in common use this is length 1.
        vals <- rep(val, length.out = sum(hit))
        result[hit] <- vals
    }

    if (length(unmatched)) {
        signal_issue(paste0(
            "The following names in 'x' did not match any split levels: ",
            paste(unique(unmatched), collapse = ", ")
        ))
    }

    result <- result[!is.na(result)]
    result
### numeric vector with expanded/reduced levels.
}, ex = function() {
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())

    refs <- levexp(Pradii03, Prings05)
    refs
})
