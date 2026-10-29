.Friedman_split_group_spec <- function(groups) {
    if (is.null(groups)) return(NULL)
    if (is.character(groups)) {
        if (length(groups) == 1L && grepl('/', groups, fixed = TRUE)) {
            groups <- strsplit(groups, '/', fixed = TRUE)[[1L]]
        }
        groups <- trimws(groups)
        groups <- groups[!is.na(groups) & nzchar(groups)]
        return(unique(groups))
    }
    as.character(groups)
}

.Friedman_choose_time <- function(cands, dat_names) {
    cands <- intersect(unique(as.character(cands)), dat_names)
    if (!length(cands)) return(character(0))
    cands[1L]
}

FriedmanForm <- structure(function # Friedman terminal specification
### This function formulates Friedman smoother specifications for
### multilevel ecological data series.
                     ##details<< Unlike \code{\link{tdForm}} and
                     ##\code{\link{lmeForm}}, this helper does not
                     ##return a regression formula. It returns a compact
                     ##terminal specification used by
                     ##\code{\link{frametoFriedman}} and
                     ##\code{\link{modelFrame}} to identify the response
                     ##series, the time variable, the grouping columns,
                     ##and the main smoothing settings.
                     ##
                     ##The time variable is the first whole-number
                     ##numeric column and the grouping columns are the
                     ##factor and character columns other than the
                     ##month column (see \code{\link{cClass}}). Roles
                     ##declared with \code{\link{defineDataRoles}} take
                     ##precedence; the metadata that
                     ##\code{\link{ringApply}} records automatically does
                     ##not. The time variable is excluded from the
                     ##grouping columns unless the declared metadata
                     ##says that time also belongs to groups, so each
                     ##series is the response over time within groups,
                     ##not within group-time combinations with fewer
                     ##than \code{min.obs} observations.
                     ##references<< Friedman, J. H. 1984. A variable span
                     ##smoother. Laboratory for Computational Statistics,
                     ##Stanford University Technical Report 5.
(
    rd, ##<<\code{data.frame}. Multilevel ecological data series.
    response = NULL, ##<<\code{NULL} or \code{character}. Name of the
                     ##response series to smooth.
    time = NULL, ##<<\code{NULL} or \code{character}. Name of the time
                 ##variable. A vector is allowed; its first element
                 ##present in \code{rd} is used.
    groups = NULL, ##<<\code{NULL} or \code{character}. Columns that
                   ##define individual series. Nested strings such as
                   ##\code{"plot/tree/sample"} are accepted.
    difference = FALSE, ##<<\code{logical}. Use subtraction instead of
                        ##ratio detrending.
    span = 'cv', ##<<\code{character} or \code{numeric}. Span argument
                 ##passed to \code{stats::supsmu()}.
    bass = 0, ##<<\code{numeric}. Bass argument passed to
              ##\code{stats::supsmu()}.
    periodic = FALSE, ##<<\code{logical}. Periodic smoother option.
    min.obs = 4L, ##<<\code{integer}. Minimum finite observations per
                  ##series.
    validate = c('none','warn','stop') ##<<\code{character}. Validation mode.
) {
    validate <- match.arg(validate)
    signal <- function(msg) {
        if (validate == 'warn') warning(msg, call. = FALSE)
        if (validate == 'stop') stop(msg, call. = FALSE)
        invisible(NULL)
    }
    `%||%` <- function(x, y) if (is.null(x)) y else x

    if (!is.data.frame(rd)) {
        signal("rd must be a data.frame")
        rd <- as.data.frame(rd)
    }

    meta <- attr(rd, 'BIOdry_meta', exact = TRUE)

    ## roles from column types (see cClass)
    guess_time <- function(dat) {
        .BIOdry_time_cols(dat)
    }
    guess_groups <- function(dat, time_var = character(0), response = character(0)) {
        setdiff(.BIOdry_group_cols(dat), c(time_var, response))
    }
    guess_response <- function(dat, time_var = character(0), group_vars = character(0)) {
        nms <- names(dat)
        if (is.null(nms)) return(character(0))
        from_attr <- attr(dat, 'x.nm', exact = TRUE)
        if (!is.null(from_attr)) {
            hit <- intersect(as.character(from_attr), nms)
            if (length(hit)) return(hit[1L])
        }
        low <- tolower(nms)
        pref <- c('ai','x','rw','trw','ring','ring_width','ringwidth','value','y')
        hit <- nms[match(pref, low, nomatch = 0L)]
        hit <- setdiff(hit[nzchar(hit)], c(time_var, group_vars))
        if (length(hit)) return(hit[1L])
        num <- nms[vapply(dat, is.numeric, logical(1))]
        num <- setdiff(num, c(time_var, group_vars))
        if (length(num)) return(num[1L])
        character(0)
    }

    meta_time_vars <- NULL
    meta_time <- NULL
    meta_groups <- NULL
    meta_resp <- NULL
    meta_time_in_groups <- FALSE
    ## metadata roles decide only when declared with defineDataRoles()
    if (is.list(meta)) {
        if (.BIOdry_declared(meta, 'time')) {
            meta_time_vars <- meta$time_vars %||% meta$time
            meta_time <- meta$time %||% .Friedman_choose_time(meta_time_vars, names(rd))
        }
        if (.BIOdry_declared(meta, 'groups'))
            meta_groups <- .Friedman_split_group_spec(meta$groups)
        if (.BIOdry_declared(meta, 'response'))
            meta_resp <- meta$active_response %||% meta$response
        if (length(meta_resp) > 1L) meta_resp <- meta_resp[1L]
        meta_time_in_groups <- isTRUE(meta$time_in_groups) &&
            length(meta_groups) && length(meta_time_vars) &&
            any(meta_groups %in% meta_time_vars)
    }

    time_explicit <- !is.null(time)
    groups_explicit <- !is.null(groups)

    if (is.null(time)) {
        time_candidates <- meta_time_vars %||% guess_time(rd)
    } else {
        time_candidates <- intersect(as.character(time), names(rd))
        if (!length(time_candidates)) {
            signal("The requested time column was not found in rd")
            time_candidates <- character(0)
        }
    }
    time_var <- .Friedman_choose_time(time_candidates, names(rd))
    if (!length(time_var) && length(meta_time)) time_var <- meta_time[1L]
    if (!length(time_var)) time_var <- character(0)

    if (is.null(response)) {
        response <- meta_resp %||% guess_response(rd, time_var = time_var)
        if (length(response) > 1L) response <- response[1L]
    } else {
        response <- intersect(as.character(response), names(rd))
        if (!length(response)) {
            signal("The requested response column was not found in rd")
            response <- NULL
        } else response <- response[1L]
    }

    if (is.null(groups)) {
        groups <- meta_groups %||% guess_groups(rd, time_var = time_var, response = response)
    } else {
        groups <- .Friedman_split_group_spec(groups)
        groups <- intersect(as.character(groups), names(rd))
    }

    time_vars <- intersect(unique(c(time_candidates, time_var)), names(rd))
    if (!meta_time_in_groups) {
        groups <- setdiff(groups, time_vars)
    }
    groups <- unique(groups)

    if (length(difference) != 1L || !is.logical(difference) || is.na(difference)) {
        signal("difference must be TRUE or FALSE")
        difference <- FALSE
    }
    if (length(periodic) != 1L || !is.logical(periodic) || is.na(periodic)) {
        signal("periodic must be TRUE or FALSE")
        periodic <- FALSE
    }
    if (!(is.character(span) && length(span) == 1L && identical(span, 'cv')) &&
        !(is.numeric(span) && length(span) == 1L && is.finite(span) && span > 0 && span <= 1)) {
        signal("span must be 'cv' or a single numeric value in (0, 1]")
        span <- 'cv'
    }
    bass <- suppressWarnings(as.numeric(bass)[1L])
    if (!is.finite(bass) || is.na(bass) || bass < 0) {
        signal("bass must be a single non-negative numeric value")
        bass <- 0
    }
    min.obs <- as.integer(min.obs)[1L]
    if (!is.finite(min.obs) || is.na(min.obs) || min.obs < 2L) {
        signal("min.obs must be an integer >= 2")
        min.obs <- 4L
    }

    out <- list(
        response = response,
        time = if (length(time_var)) time_var else NULL,
        time_vars = time_vars,
        groups = groups,
        time_in_groups = meta_time_in_groups || (groups_explicit && any(groups %in% time_vars)),
        difference = difference,
        span = span,
        bass = bass,
        periodic = periodic,
        min.obs = min.obs
    )
    class(out) <- c('FriedmanForm', 'list')
    out
### \code{list} inheriting from class \code{"FriedmanForm"}.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03):
    ## relative time within trees, then cumulative growth scaled to the radii
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    dfm2 <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum')
    ## the specification resolved from the data: response, time, series
    ## and smoothing settings
    FriedmanForm(dfm2) |> str()
    ## a difference index (x - smooth) instead of a ratio index (x / smooth)
    FriedmanForm(dfm2, difference = TRUE)$difference
})
