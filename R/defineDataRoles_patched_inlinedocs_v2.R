.BIOdry_signal <- function(validate = c("none", "warn", "stop"), msg) {
    validate <- match.arg(validate)
    if (identical(validate, "warn")) warning(msg, call. = FALSE)
    if (identical(validate, "stop")) stop(msg, call. = FALSE)
    invisible(NULL)
}

.BIOdry_split_group_spec <- function(groups) {
    if (is.null(groups)) return(NULL)
    if (is.numeric(groups)) return(as.integer(groups))
    if (is.character(groups)) {
        if (length(groups) == 1L && grepl("/", groups, fixed = TRUE)) {
            groups <- strsplit(groups, "/", fixed = TRUE)[[1L]]
        }
        groups <- trimws(groups)
        groups <- groups[!is.na(groups) & nzchar(groups)]
        return(unique(groups))
    }
    stop("'groups' must be NULL, numeric, or character.", call. = FALSE)
}

.BIOdry_infer_time_var <- function(rd, groups = NULL) {
    nms <- names(rd)
    if (is.null(nms) || !length(nms)) return(NULL)
    groups <- intersect(as.character(groups), nms)

    ## first whole-number column, as in BIOdry 0.9.1
    whole <- setdiff(.BIOdry_time_cols(rd), groups)
    if (length(whole)) return(whole[1L])

    date_like <- nms[vapply(rd, function(z) inherits(z, c("Date", "POSIXct", "POSIXlt", "POSIXt")), logical(1L))]
    date_like <- setdiff(date_like, groups)
    if (length(date_like)) return(date_like[1L])

    NULL
}

.BIOdry_choose_primary_time <- function(time_vars) {
    if (is.null(time_vars) || !length(time_vars)) return(NULL)
    time_vars[1L]
}

.BIOdry_infer_group_vars <- function(rd, time = NULL) {
    nms <- names(rd)
    if (is.null(nms) || !length(nms)) return(character(0L))
    time <- intersect(as.character(time), nms)
    ## factor and character columns that are not months, outer level first
    ## (the reverse of the column order, as in lmeForm and tdForm)
    rev(setdiff(.BIOdry_group_cols(rd), time))
}

.BIOdry_infer_inputs <- function(rd, time = NULL, groups = NULL) {
    nms <- names(rd)
    if (is.null(nms) || !length(nms)) return(character(0L))
    exclude <- unique(c(as.character(time), as.character(groups)))
    out <- nms[vapply(seq_along(rd), function(i) {
        (is.numeric(rd[[i]]) || is.integer(rd[[i]])) && !(nms[i] %in% exclude)
    }, logical(1L))]
    unique(out)
}

.BIOdry_normalize_name_map <- function(name_map, rd = NULL) {
    if (is.null(name_map)) return(setNames(character(0L), character(0L)))
    if (!is.character(name_map)) {
        stop("'name_map' must be a named character vector.", call. = FALSE)
    }
    if (is.null(names(name_map)) || any(!nzchar(names(name_map)))) {
        stop("'name_map' must be a named character vector.", call. = FALSE)
    }
    name_map <- name_map[!is.na(name_map) & nzchar(name_map)]
    name_map <- name_map[!is.na(names(name_map)) & nzchar(names(name_map))]
    if (!is.null(rd)) {
        vals_in_rd <- unname(name_map) %in% names(rd)
        if (any(vals_in_rd)) name_map <- name_map[vals_in_rd]
    }
    name_map
}

defineDataRoles <- structure(function
### Constructor for BIOdry role metadata
### Attach variable-role metadata to multilevel ecological data
                           ##details<< This constructor attaches a small
                           ##\code{BIOdry_meta} attribute to a
                           ##\code{data.frame} so that later workflow
                           ##steps can use declared roles. Functions such as
                           ##\code{\link{lmeForm}},
                           ##\code{\link{tdForm}},
                           ##\code{\link{frametoLme}}, and
                           ##\code{\link{FriedmanForm}} decide roles from
                           ##column types (see \code{\link{cClass}}); a
                           ##\code{time}, \code{groups} or \code{response}
                           ##declared here takes precedence over that rule.
                           ##Roles that are only inferred, and the metadata
                           ##that \code{\link{ringApply}} records
                           ##automatically, do not.
                           ##
                           ##The constructor respects explicit user
                           ##definitions first. If the user supplies
                           ##\code{time}, \code{groups},
                           ##\code{inputs}, or \code{response}, these
                           ##values are stored as given after
                           ##validation. If they are not supplied, the
                           ##constructor infers them from column types:
                           ##the time variable is the first whole-number
                           ##numeric column, the groups are the factor and
                           ##character columns other than the month column,
                           ##outer level first, and the inputs are the
                           ##remaining numeric columns. A time variable is
                           ##not also placed in the grouping hierarchy
                           ##unless the user includes it in \code{groups}.
                           ##
                           ##The \code{time} argument may be given as
                           ##a single variable or as a character vector
                           ##describing ordered time components such as
                           ##\code{c("month", "year")}. In that case,
                           ##all supplied names are stored in
                           ##\code{time_vars}, while a conservative
                           ##primary time variable, the first supplied
                           ##element, is also stored in \code{time} for
                           ##downstream functions that expect a single
                           ##covariate name.
                           ##
                           ##The \code{groups} argument may be given as
                           ##a character vector such as
                           ##\code{c("plot", "tree", "sample")} or as a
                           ##nested string such as
                           ##\code{"plot/tree/sample"}. Whole-number
                           ##columns, such as \code{year}, are grouping
                           ##levels only when declared here; to use
                           ##\code{year} as a random grouping level, see
                           ##\code{\link{frametoLme}}.
                           ##
                           ##The constructor is intentionally light. It
                           ##does not copy the data, change user-visible
                           ##columns, or fit any models. It only stores
                           ##compact metadata describing: raw input
                           ##measurement variables, the current active
                           ##response when available, the primary time
                           ##variable, all declared time components, the
                           ##grouping hierarchy, whether time was also
                           ##declared as part of that hierarchy, and an
                           ##optional \code{name_map} linking older
                           ##names to derived names such as
                           ##\code{year -> hydro_year_id}. This allows
                           ##later functions to preserve context across
                           ##successive transformations.
                           ##
                           ##When automatic inference is not possible,
                           ##the function fails gracefully according to
                           ##\code{validate}. Use
                           ##\code{validate = "warn"} to see a
                           ##diagnostic while still returning the data
                           ##with partial metadata, or
                           ##\code{validate = "stop"} to require a
                           ##fully resolved declaration.
(
    rd, ##<< \code{data.frame}. Multilevel ecological or climatic data.
    inputs = NULL, ##<< Optional \code{character} vector of raw input or
                   ##measurement variables. For climate data this can
                   ##contain more than one variable, for example
                   ##\code{c("pr", "tm")}; for \code{\link{wlai}}
                   ##precipitation comes first (columns named \code{pr}
                   ##and \code{tm} are used by name). If omitted, numeric
                   ##non-group, non-time columns are inferred.
    response = NULL, ##<< Optional \code{character} scalar giving the
                     ##current active response variable. If omitted and
                     ##exactly one input variable is inferred, that
                     ##variable is used as the response; otherwise the
                     ##response remains unset.
    time = NULL, ##<< Optional \code{character} vector naming one or
                 ##more ordered time variables. Examples include
                 ##\code{"year"} or \code{c("month", "year")}. The
                 ##constructor stores all supplied names in
                 ##\code{time_vars} and the first one as the primary
                 ##time variable in \code{time}.
    groups = NULL, ##<< Optional grouping hierarchy, either as a
                   ##\code{character} vector or as a nested string
                   ##with \code{/} separators such as
                   ##\code{"plot/tree/sample"}. If omitted, the factor
                   ##and character columns other than the month column
                   ##are inferred, outer level first.
    name_map = NULL, ##<< Optional named \code{character} vector mapping
                     ##older variable names to current names, for
                     ##example \code{c(year = "hydro_year_id")}. This
                     ##mapping is stored verbatim and can be updated by
                     ##later workflow steps.
    validate = c("warn", "stop", "none") ##<< Validation mode. Use
                     ##\code{"warn"} to keep partial metadata with a
                     ##diagnostic, \code{"stop"} to require a fully
                     ##resolved declaration, or \code{"none"} for
                     ##quiet inference.
)
{
    validate <- match.arg(validate)

    if (!is.data.frame(rd)) stop("'rd' must be a data.frame.", call. = FALSE)
    nms <- names(rd)
    if (is.null(nms) || !length(nms)) stop("'rd' must have named columns.", call. = FALSE)

    groups_in <- .BIOdry_split_group_spec(groups)
    if (is.numeric(groups_in)) {
        idx <- unique(as.integer(groups_in[is.finite(groups_in)]))
        if (!length(idx) || any(idx < 1L) || any(idx > ncol(rd))) {
            stop("Numeric 'groups' indices are out of bounds for 'rd'.", call. = FALSE)
        }
        groups_in <- nms[idx]
    }

    if (!is.null(time)) {
        if (!is.character(time)) {
            stop("'time' must be a character vector when supplied.", call. = FALSE)
        }
        time <- unique(trimws(time))
        time <- time[!is.na(time) & nzchar(time)]
        if (!length(time)) {
            stop("'time' must contain at least one non-empty character string.", call. = FALSE)
        }
        miss <- setdiff(time, nms)
        if (length(miss)) {
            .BIOdry_signal(validate, paste0("Unknown time variable(s): ", paste(miss, collapse = ", ")))
            time <- intersect(time, nms)
        }
        if (!length(time)) time <- NULL
    }
    if (is.null(time)) {
        inferred_time <- .BIOdry_infer_time_var(rd, groups = groups_in)
        time_vars <- if (is.null(inferred_time)) character(0L) else inferred_time
    } else {
        time_vars <- time
    }
    if (!length(time_vars)) .BIOdry_signal(validate, "Could not infer a unique time variable.")
    primary_time <- .BIOdry_choose_primary_time(time_vars)

    if (!is.null(groups_in)) {
        miss <- setdiff(groups_in, nms)
        if (length(miss)) {
            stop(paste0("Unknown grouping columns: ", paste(miss, collapse = ", ")), call. = FALSE)
        }
        groups_out <- unique(groups_in)
    } else {
        groups_out <- .BIOdry_infer_group_vars(rd, time = time_vars)
    }

    time_in_groups <- FALSE
    if (length(time_vars) && length(groups_out)) {
        time_in_groups <- any(time_vars %in% groups_out)
        if (!time_in_groups) groups_out <- setdiff(groups_out, time_vars)
    }

    if (!is.null(inputs)) {
        if (!is.character(inputs)) stop("'inputs' must be a character vector when supplied.", call. = FALSE)
        inputs <- unique(inputs[!is.na(inputs) & nzchar(inputs)])
        miss <- setdiff(inputs, nms)
        if (length(miss)) {
            stop(paste0("Unknown input variable(s): ", paste(miss, collapse = ", ")), call. = FALSE)
        }
        inputs_out <- inputs
    } else {
        inputs_out <- .BIOdry_infer_inputs(rd, time = time_vars, groups = groups_out)
    }

    if (!is.null(response)) {
        if (!is.character(response)) {
            stop("'response' must be a character vector when supplied.", call. = FALSE)
        }
        response <- unique(response[!is.na(response) & nzchar(response)])
        miss <- setdiff(response, nms)
        if (length(miss)) {
            stop(paste0("Unknown response variable(s): ", paste(miss, collapse = ", ")), call. = FALSE)
        }
        response_out <- response
    } else {
        response_out <- if ("AI" %in% nms) "AI" else if (length(inputs_out) == 1L) inputs_out[1L] else character(0L)
    }

    active_response <- if (length(response_out) == 1L) response_out else if ("AI" %in% response_out) "AI" else NULL

    name_map <- .BIOdry_normalize_name_map(name_map)

    meta <- list(
        inputs = inputs_out,
        response = response_out,
        active_response = active_response,
        time = primary_time,
        time_vars = time_vars,
        groups = groups_out,
        time_in_groups = isTRUE(time_in_groups),
        name_map = name_map,
        call = match.call()
    )
    class(meta) <- c("BIOdry_meta", "list")

    attr(rd, "BIOdry_meta") <- meta
    rd
### \code{data.frame} with a \code{BIOdry_meta} attribute.
},
ex = function() {
    ## tree-ring widths: declare the time and the grouping hierarchy
    data(Prings05, envir = environment())
    dr1 <- defineDataRoles(Prings05, time = "year",
                           groups = "plot/tree/sample")
    str(attr(dr1, "BIOdry_meta"))
    ## declared roles are used by the formula helpers
    lmeForm(dr1)

    ## monthly climate: precipitation first, then temperature, as wlai()
    ## reads them; year and month as time components
    data(PTclim05, envir = environment())
    cl1 <- defineDataRoles(PTclim05, inputs = c("pr", "tm"),
                           time = c("year", "month"), groups = "plot")
    str(attr(cl1, "BIOdry_meta"))
})
