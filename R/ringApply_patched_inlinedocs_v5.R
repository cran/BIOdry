ringApply <- structure(function#Multilevel apply
### Wrapper of \code{\link{Map}} to apply functions on multilevel data
### series and preserve factor-level structure in the outputs.
                       ##details<< Other functions such as
                       ##\code{\link{rtimes}}, \code{\link{scacum}},
                       ##\code{\link{amod}}, or \code{\link{wlai}} can
                       ##be implemented. Function arguments should be
                       ##formulated as suggested in
                       ##\code{\link{mapply}}, with constant
                       ##arguments being stored in a \code{MoreArgs}
                       ##list. This implementation keeps
                       ##\code{MoreArgs} constant across split blocks.
                       ##The split levels are the factor and character
                       ##columns other than the month column (see
                       ##\code{\link{cClass}}); whole-number columns such
                       ##as \code{year} are split levels only when
                       ##requested through \code{lv}. The nesting
                       ##structure is resolved
                       ##through \code{\link{slitFrame}}, which is the
                       ##infrastructure helper used to materialize the
                       ##hierarchy defined by \code{lv}. When
                       ##\code{lv} is supplied, whether as a numeric
                       ##position or as a character level name, that
                       ##choice controls the split hierarchy
                       ##explicitly.
                       ##
                       ##After splitting and before applying
                       ##\code{fn}, each block is ordered on the time
                       ##variables that remain inside the block.
                       ##Variables used as split levels are excluded
                       ##from the ordering keys. Thus a call such as
                       ##\code{lv = 'year'} orders within yearly
                       ##blocks by finer time variables such as month,
                       ##day, or relative time rather than by year
                       ##again, whereas a call such as
                       ##\code{lv = 'sample'} orders within each ring
                       ##series by year and then by any finer time
                       ##variable. Ordering prefers
                       ##\code{hydro_year_id}, then year-like
                       ##variables not used for splitting, then
                       ##month-like variables, and finally relative
                       ##time variables such as \code{time},
                       ##\code{age}, or \code{cambial_age}. This
                       ##contract is important because other BIOdry
                       ##functions can rely on \code{ringApply} to
                       ##deliver already split and time-ordered
                       ##blocks. Split variables used internally for a
                       ##transformation step are recorded in the
                       ##metadata history, but they are not promoted
                       ##automatically to the persistent grouping
                       ##hierarchy used later by
                       ##\code{frametoLme} or
                       ##\code{frametoFriedman}. In particular, a
                       ##derived time key such as
                       ##\code{hydro_year_id} may be the active time
                       ##variable after \code{moveYr} while remaining
                       ##outside the grouping hierarchy unless the user
                       ##explicitly requested otherwise in
                       ##\code{defineDataRoles}.
                       ##
                       ##Duplicated output column names are collapsed
                       ##when they are exact duplicates. Output
                       ##columns are ordered explicitly with record or
                       ##measured variables first (for example
                       ##\code{x}, \code{csx}, \code{pr},
                       ##\code{pm}, or \code{tm}), then time
                       ##variables (for example hour, day, week,
                       ##month, or year), and finally grouping
                       ##factors ordered from the lowest available
                       ##hierarchy to the broadest hierarchy
                       ##(for example \code{sample} before
                       ##\code{tree}, \code{plot}, \code{zone}, or
                       ##\code{biome}). By default, validation is
                       ##silent for backwards compatibility; use
                       ##\code{validate = "warn"} or
                       ##\code{validate = "stop"} for stricter
                       ##diagnostics.
                       ##
                       ##This patched version also attaches a small
                       ##workflow attribute named
                       ##\code{"BIOdry_meta"} to the returned data
                       ##frame. The attribute stores lightweight
                       ##context from the current evaluation, such as
                       ##the applied function name, the split levels,
                       ##a compact summary of relevant arguments,
                       ##simple role hints (response, time, grouping
                       ##variables), and a conservative variable-name
                       ##map when a known transformation creates a new
                       ##key, for example \code{year ->
                       ##hydro_year_id} after \code{moveYr}. The role
                       ##hints are taken from column types and record
                       ##context only: they do not override the type
                       ##rule in \code{lmeForm}, \code{tdForm},
                       ##\code{frametoLme} or \code{FriedmanForm}; only
                       ##roles declared with \code{\link{defineDataRoles}}
                       ##do. The attribute does not alter the returned
                       ##columns.
                       ##
                       ##When constructor metadata or prior
                       ##
                       ##\code{ringApply} steps indicate that a
                       ##time variable has been replaced by a
                       ##derived annual key, character values of
                       ##
                       ##\code{lv} are resolved against that
                       ##current key before splitting. Thus a
                       ##workflow such as
                       ##
                       ##\code{moveYr -> wlai} can keep using
                       ##
                       ##\code{lv = 'year'} while internally
                       ##splitting on 
                       ##\code{hydro_year_id} when
                       ##that derived variable has become the
                       ##active annual index. The original
                       ##calendar 
                       ##\code{year} column is retained
                       ##for reference when the applied function
                       ##returns it.
                       ##
                       ## For climatic long-format data, a safer
                       ## workflow is often to first use
                       ##\code{\link{moveYr}} to create a stable annual
                       ## key such as \code{hydro_year_id}, then split
                       ## by the relevant factor columns plus that year
                       ## key.

                       ##
                       ## This version also adds column-focused dispatch
                       ## through the \code{focus} argument. For core
                       ## BIOdry helpers such as
                       ## \code{rtimes}, \code{scacum}, \code{amod},
                       ## \code{moveYr}, or \code{wlai}, the default
                       ## behavior remains to pass the full split data
                       ## frame for backwards compatibility. For other
                       ## helpers, \code{ringApply} now defaults to the
                       ## active response stored in
                       ## \code{BIOdry_meta}. Users may override this
                       ## with \code{focus}, for example a single
                       ## column, several columns, or the special value
                       ## \code{".data"} to force full-data-frame
                       ## dispatch. The companion argument
                       ## \code{focus_mode} controls how several focused
                       ## columns are passed: as a single data frame, as
                       ## separate vector arguments, or by automatic
                       ## selection. A scalar vector result of the same
                       ## length as the split block is written back to
                       ## the focused column by default, or to
                       ## \code{target} when that argument is
                       ## supplied.

                       ##references<< Lara, W., F. Bravo,
                       ##D. Maguire. 2013. Modeling patterns between
                       ##drought and tree biomass growth from
                       ##dendrochronological data: A multilevel
                       ##approach. Agric. For. Meteorol.,
                       ##178-179:140-151.
(
    rd, ##<<\code{data.frame}. Multilevel ecological data series.
    lv = 1, ##<< \code{numeric} position, or \code{character} name, of an
            ##ecological factor in the processed MEDS.
    fn = 'scacum', ##<< \code{character} name of the function to be
                   ##evaluated (see details). Default 'scacum'
                   ##computes scaled-cumulative radii.
    focus = NULL, ##<< Optional focus column or columns. Use
                  ##\code{NULL} to keep the historical full-data
                  ##dispatch for core BIOdry helpers and otherwise
                  ##default to the active response stored in
                  ##\code{BIOdry_meta}. Use \code{".data"} to
                  ##force full-data-frame dispatch.
    target = NULL, ##<< Optional output column name used when a
                   ##focused helper returns a single vector of the
                   ##same length as the split block. By default the
                   ##focused column is replaced when a single focus
                   ##column is used.
    focus_mode = c("auto","vector","args","data.frame"), ##<<
                   ##How to pass focused columns to \code{fn}. In
                   ##\code{"auto"} mode, one focused column is
                   ##passed as a vector, two columns are passed as
                   ##two vector arguments, and three or more columns
                   ##are passed as a small data frame.
    validate = c("none","warn","stop"), ##<< Validation mode. Use
            ##\code{"none"} for quiet backward-compatible behavior,
            ##\code{"warn"} for diagnostics, or \code{"stop"} for
            ##strict checking.
    ... ##<< Further arguments in the function being specified
        ##\code{fn} argument (see details)
)
{
    lv_input <- lv

    .ringApply_signal <- function(msg){
        mode <- validate[1L]
        if(identical(mode, "warn")) warning(msg, call. = FALSE)
        if(identical(mode, "stop")) stop(msg, call. = FALSE)
        invisible(NULL)
    }

    .biodry_get_meta <- function(x){
        meta <- attr(x, "BIOdry_meta", exact = TRUE)
        if(is.null(meta) || !is.list(meta)) meta <- list()
        meta
    }

    .biodry_bootstrap_meta <- function(x){
        meta <- .biodry_get_meta(x)
        if(length(meta)) return(meta)
        if(exists("defineDataRoles", mode = "function")){
            boot <- tryCatch(
                defineDataRoles(x, validate = "none"),
                error = function(e) NULL
            )
            meta2 <- .biodry_get_meta(boot)
            if(length(meta2)) return(meta2)
        }
        list()
    }

    .biodry_map_to_named_character <- function(x){
        if(is.null(x)) return(setNames(character(0L), character(0L)))
        if(is.list(x)){
            if(!length(x)) return(setNames(character(0L), character(0L)))
            nms <- names(x)
            vals <- vapply(x, function(z){
                if(is.null(z) || !length(z) || is.na(z[1L])) return(NA_character_)
                as.character(z[1L])
            }, character(1L))
            keep <- !is.na(vals) & nzchar(vals) & !is.null(nms) & nzchar(nms)
            return(stats::setNames(vals[keep], nms[keep]))
        }
        if(is.character(x)){
            nms <- names(x)
            if(is.null(nms)) return(setNames(character(0L), character(0L)))
            keep <- !is.na(x) & nzchar(x) & nzchar(nms)
            return(stats::setNames(unname(x[keep]), nms[keep]))
        }
        setNames(character(0L), character(0L))
    }

    .biodry_resolve_lv_name <- function(lv_name, rd_now, meta, fn_name){
        if(!is.character(lv_name) || length(lv_name) != 1L || is.na(lv_name) || !nzchar(lv_name)){
            return(lv_name)
        }
        nms_now <- names(rd_now)
        nm_map <- .biodry_map_to_named_character(meta$name_map)
        if(!length(nm_map) || !(lv_name %in% names(nm_map))) return(lv_name)
        target <- unname(nm_map[[lv_name]])
        if(!length(target) || is.na(target) || !nzchar(target) || !(target %in% nms_now)) return(lv_name)
        if(!(lv_name %in% nms_now)) return(target)

        time_vars <- meta$time_vars
        if(is.null(time_vars)) time_vars <- character(0L)
        current_time <- meta$time
        if(!is.character(current_time) || length(current_time) != 1L || is.na(current_time) || !nzchar(current_time) || !(current_time %in% nms_now)){
            current_time <- NULL
        }

        if(length(current_time) == 1L && identical(target, current_time) && lv_name %in% time_vars){
            return(target)
        }

        if(identical(fn_name, "wlai") && identical(lv_name, "year") && identical(target, "hydro_year_id")){
            return(target)
        }

        lv_name
    }

    .biodry_parse_focus <- function(focus){
        if(is.null(focus)) return(NULL)
        if(is.character(focus) && length(focus) == 1L && !is.na(focus) && grepl("/", focus, fixed = TRUE)) {
            focus <- strsplit(focus, "/", fixed = TRUE)[[1L]]
        }
        focus <- as.character(focus)
        focus <- trimws(focus)
        focus[nzchar(focus)]
    }

    .biodry_resolve_focus <- function(focus, rd_now, meta, fn_name){
        core_df_fns <- c("rtimes", "scacum", "amod", "moveYr", "wlai")
        if(is.null(focus) || !length(focus)){
            if(fn_name %in% core_df_fns) return(".data")
            active_response <- meta$active_response
            if(is.character(active_response) && length(active_response) == 1L && !is.na(active_response) &&
               nzchar(active_response) && active_response %in% names(rd_now)) {
                return(active_response)
            }
            role_response <- meta$role_hints$response
            if(is.character(role_response) && length(role_response) && role_response[1L] %in% names(rd_now)) {
                return(role_response[1L])
            }
            return(".data")
        }
        focus <- .biodry_parse_focus(focus)
        if(identical(focus, ".data")) return(".data")
        nm_map <- .biodry_map_to_named_character(meta$name_map)
        focus <- vapply(focus, function(ff){
            if(ff %in% names(rd_now)) return(ff)
            if(ff %in% names(nm_map) && unname(nm_map[[ff]]) %in% names(rd_now)) return(unname(nm_map[[ff]]))
            ff
        }, character(1L))
        missing_focus <- setdiff(focus, names(rd_now))
        if(length(missing_focus)) {
            stop("Unknown focus columns: ", paste(missing_focus, collapse = ", "), call. = FALSE)
        }
        unique(focus)
    }

    .biodry_dispatch_mode <- function(focus_names, focus_mode){
        if(identical(focus_names, ".data")) return("data.frame")
        if(identical(focus_mode, "data.frame")) return("data.frame")
        if(identical(focus_mode, "vector")) return("vector")
        if(identical(focus_mode, "args")) return("args")
        if(length(focus_names) <= 1L) return("vector")
        if(length(focus_names) == 2L) return("args")
        "data.frame"
    }

    .biodry_finalize_result <- function(block_i, res, focus_names, target_name = NULL){
        if(is.null(res)) return(NULL)
        if(identical(focus_names, ".data")) return(res)

        if(is.atomic(res) || is.factor(res)){
            if(length(res) == nrow(block_i)){
                out <- block_i
                nm <- target_name
                if(is.null(nm) || !nzchar(nm)){
                    if(length(focus_names) == 1L) nm <- focus_names[1L]
                }
                if(is.null(nm) || !nzchar(nm)){
                    stop("Focused helper returned a single vector but no output column name could be resolved. Supply 'target' or return a data.frame.", call. = FALSE)
                }
                out[[nm]] <- res
                return(out)
            }
            return(res)
        }

        if(is.matrix(res)) res <- as.data.frame(res, stringsAsFactors = FALSE)

        if(is.data.frame(res)){
            if(nrow(res) == nrow(block_i)){
                out <- block_i
                for(nm in names(res)) out[[nm]] <- res[[nm]]
                return(out)
            }
            return(res)
        }

        if(is.list(res) && length(res)){
            lens <- vapply(res, length, integer(1L))
            if(all(lens == nrow(block_i))){
                df <- as.data.frame(res, stringsAsFactors = FALSE)
                out <- block_i
                for(nm in names(df)) out[[nm]] <- df[[nm]]
                return(out)
            }
        }

        res
    }

    .biodry_compact_arg <- function(x){
        if(is.null(x)) return(NULL)
        if(is.atomic(x) && length(x) <= 5L) return(x)
        if(is.factor(x) && length(x) <= 5L) return(as.character(x))
        if(is.character(x) && length(x) <= 5L) return(x)
        cls <- class(x)[1L]
        len <- tryCatch(length(x), error = function(e) NA_integer_)
        if(is.na(len)) return(paste0("<", cls, ">"))
        paste0("<", cls, ":", len, ">")
    }

    .biodry_arg_summary <- function(dotargs, MoreArgs){
        out <- list()
        if(length(dotargs)){
            out$dotargs <- lapply(dotargs, .biodry_compact_arg)
        }
        if(!is.null(MoreArgs)){
            out$MoreArgs <- lapply(MoreArgs, .biodry_compact_arg)
        }
        out
    }

    .biodry_role_hints <- function(d, split_vars = character(0L)){
        nms <- names(d)
        first_present <- function(candidates){
            hit <- candidates[candidates %in% nms]
            if(length(hit)) hit[1L] else NULL
        }
        response <- first_present(c("AI", "x", "csx", "pr", "pm", "tm"))
        ## roles from column types (see cClass)
        time <- .BIOdry_time_cols(d)[1L]
        if(is.na(time)) time <- NULL
        groups <- setdiff(unique(c(split_vars, rev(.BIOdry_group_cols(d)))),
                          .BIOdry_time_cols(d))
        if(!is.null(time) && nzchar(time)) groups <- setdiff(groups, time)
        list(response = response, time = time, groups = groups)
    }

    .biodry_name_map <- function(rd_in, rd_out, fn_name){
        in_nms <- names(rd_in)
        out_nms <- names(rd_out)
        nm_map <- list()
        if(identical(fn_name, "moveYr")){
            if("year" %in% in_nms && "hydro_year_id" %in% out_nms){
                nm_map$year <- "hydro_year_id"
            }
        }
        if(identical(fn_name, "wlai")){
            if("AI" %in% out_nms) nm_map$AI <- "AI"
            if("year" %in% in_nms && "hydro_year_id" %in% out_nms && !"year" %in% out_nms){
                nm_map$year <- "hydro_year_id"
            }
        }
        nm_map
    }

    .biodry_merge_name_map <- function(old_map, new_map){
        if(is.null(old_map) || !is.list(old_map)) old_map <- list()
        if(is.null(new_map) || !is.list(new_map) || !length(new_map)) return(old_map)
        old_map[names(new_map)] <- new_map
        old_map
    }

    validate <- match.arg(validate)
    focus_mode <- match.arg(focus_mode)

    if(!is.data.frame(rd)){
        stop("'rd' must be a data.frame.")
    }
    if(!is.character(fn) || length(fn) != 1L || is.na(fn) || !nzchar(fn)){
        stop("'fn' must be a single non-empty character string.")
    }
    if(!exists(fn, mode = "function")){
        stop("Function '", fn, "' was not found.")
    }

    old_meta <- .biodry_bootstrap_meta(rd)
    if(length(old_meta)) attr(rd, "BIOdry_meta") <- old_meta

    nms <- names(rd)
    if(is.null(nms) || !length(nms)){
        stop("'rd' must have named columns.")
    }

    is_month_vector <- function(x, nm = ""){
        y <- x
        if(inherits(y, c("Date", "POSIXt"))) return(FALSE)
        if(is.factor(y)) y <- as.character(y)
        if(is.character(y)){
            ys <- trimws(y)
            ys <- ys[!is.na(ys) & nzchar(ys)]
            if(!length(ys)) return(FALSE)
            ys3 <- tolower(substr(ys, 1L, 3L))
            abb3 <- tolower(month.abb)
            full3 <- tolower(substr(month.name, 1L, 3L))
            if(all(ys3 %in% abb3) || all(ys3 %in% full3)) return(TRUE)
            suppressWarnings(yn <- as.integer(ys))
            yn <- yn[!is.na(yn)]
            if(length(yn) && all(yn %in% 1:12)) return(TRUE)
            return(FALSE)
        }
        if(is.numeric(y) || is.integer(y)){
            yi <- y[!is.na(y)]
            if(!length(yi)) return(FALSE)
            nm_l <- tolower(nm)
            month_name_hint <- nm_l %in% c("month","mnt","mo","mon")
            vals_are_months <- all(yi %in% 1:12)
            if(month_name_hint && vals_are_months) return(TRUE)
            return(FALSE)
        }
        FALSE
    }

    month_index <- function(x, nm = ""){
        y <- x
        if(inherits(y, c("Date", "POSIXt"))) return(rep(NA_integer_, length(y)))
        if(is.factor(y)) y <- as.character(y)
        if(is.character(y)) {
            ys <- trimws(y)
            ys3 <- tolower(substr(ys, 1L, 3L))
            abb3 <- tolower(month.abb)
            full3 <- tolower(substr(month.name, 1L, 3L))
            out <- match(ys3, abb3)
            miss <- is.na(out)
            out[miss] <- match(ys3[miss], full3)
            miss <- is.na(out)
            suppressWarnings(yn <- as.integer(ys[miss]))
            out[miss] <- yn
            out[!(out %in% 1:12)] <- NA_integer_
            return(as.integer(out))
        }
        if(is.numeric(y) || is.integer(y)) {
            yi <- suppressWarnings(as.integer(y))
            yi[!(yi %in% 1:12)] <- NA_integer_
            nm_l <- tolower(nm)
            if(nm_l %in% c("month","mnt","mo","mon")) return(yi)
            return(rep(NA_integer_, length(y)))
        }
        rep(NA_integer_, length(y))
    }

    order_block_time <- function(d, split_vars = character(0L)){
        if(!is.data.frame(d) || !nrow(d)) return(d)
        nms_d <- names(d)
        split_vars <- intersect(split_vars, nms_d)
        keys <- list()
        add_key <- function(v) {
            if(length(v) == nrow(d)) keys[[length(keys)+1L]] <<- v
        }
        usable <- function(nm) nm %in% nms_d && !(nm %in% split_vars)

        for(nm in c("hydro_year_id", "year", "yr", "calendar_year", "hydro_year",
                    "time", "age", "cambial_age", "rel_time", "ring_time")) {
            if(usable(nm)) {
                v <- d[[nm]]
                if(is.factor(v)) v <- as.character(v)
                if(is.character(v)) suppressWarnings(v <- as.numeric(v))
                add_key(v)
            }
        }

        mcol <- NULL
        explicit_month <- tolower(nms_d) %in% c("month","mnt","mo","mon")
        explicit_month <- explicit_month & !(nms_d %in% split_vars)
        if(any(explicit_month)) {
            mcol <- nms_d[match(TRUE, explicit_month)]
        } else {
            cand <- vapply(seq_along(d), function(i)
                !(nms_d[i] %in% split_vars) && is_month_vector(d[[i]], nms_d[i]), logical(1L))
            if(any(cand)) mcol <- nms_d[which(cand)[1L]]
        }
        if(!is.null(mcol)) add_key(month_index(d[[mcol]], mcol))

        if(!length(keys)) return(d)
        ord_args <- c(keys, list(seq_len(nrow(d)), na.last = TRUE, method = "radix"))
        o <- do.call(order, ord_args)
        d[o, , drop = FALSE]
    }

    order_output_columns <- function(d, split_vars = character(0L), all_group_vars = character(0L)){
        if(!is.data.frame(d) || !ncol(d)) return(d)
        nms_d <- names(d)
        lower <- tolower(nms_d)

        time_pref <- c("hour","hr","minute","min","day","doy","yday","week","wk","month","mon","mnt","year","yr","calendar_year","hydro_year_id","hydro_year","time","rel_time","ring_time","cambial_age","age")
        outcome_pref <- c("x","csx","pr","pm","ppt","prec","precip","tm","tmean","temp","temperature")

        is_time_name <- function(nm) tolower(nm) %in% time_pref
        is_group_like <- function(v, nm){
            nm %in% unique(c(split_vars, all_group_vars)) ||
                is.factor(v) || is.character(v) || is.logical(v)
        }

        present_group_like <- nms_d[vapply(seq_along(d), function(i) is_group_like(d[[i]], nms_d[i]), logical(1L))]
        group_cols <- unique(c(intersect(all_group_vars, present_group_like),
                               intersect(split_vars, present_group_like),
                               setdiff(present_group_like, unique(c(all_group_vars, split_vars)))))

        time_cols_all <- nms_d[vapply(seq_along(d), function(i){
            inherits(d[[i]], c("Date","POSIXt")) || is_time_name(nms_d[i])
        }, logical(1L))]
        time_cols <- setdiff(time_cols_all, group_cols)
        if(length(time_cols)){
            pref_match <- match(tolower(time_cols), time_pref)
            pref_match[is.na(pref_match)] <- length(time_pref) + seq_len(sum(is.na(pref_match)))
            time_cols <- time_cols[order(pref_match, seq_along(time_cols))]
        }

        numeric_non_time <- nms_d[vapply(seq_along(d), function(i){
            (is.numeric(d[[i]]) || is.integer(d[[i]])) &&
            !(nms_d[i] %in% group_cols) && !(nms_d[i] %in% time_cols)
        }, logical(1L))]
        if(length(numeric_non_time)){
            pref_match <- match(tolower(numeric_non_time), outcome_pref)
            pref_match[is.na(pref_match)] <- length(outcome_pref) + seq_len(sum(is.na(pref_match)))
            numeric_non_time <- numeric_non_time[order(pref_match, seq_along(numeric_non_time))]
        }

        rest <- setdiff(nms_d, c(numeric_non_time, time_cols, group_cols))
        d[, c(numeric_non_time, time_cols, group_cols, rest), drop = FALSE]
    }

    ## factor and character columns that are not months (see cClass)
    levs <- .BIOdry_group_cols(rd)

    if(!length(levs)){
        if(exists("cClass", mode = "function")){
            try({
                levs <- cClass(rd, "factor")
            }, silent = TRUE)
        }
    }

    all_group_vars <- levs

    if(is.character(lv)){
        if(length(lv) != 1L || is.na(lv) || !nzchar(lv)){
            stop("'lv' must be a single valid column name when character.")
        }
        lv_resolved <- .biodry_resolve_lv_name(lv, rd, old_meta, fn)
        if(!lv_resolved %in% nms){
            stop("Requested split level '", lv, "' is not a column in 'rd'.")
        }
        if(!lv_resolved %in% levs){
            levs <- c(lv_resolved, levs)
            all_group_vars <- unique(c(lv_resolved, all_group_vars))
            .ringApply_signal(paste0("Requested level '", lv,
                              "' was not detected automatically and was added explicitly",
                              if(!identical(lv_resolved, lv)) paste0(" as '", lv_resolved, "'") else "",
                              "."))
        }
        lv <- match(lv_resolved, levs)
    }

    if(is.numeric(lv)){
        if(length(lv) != 1L || is.na(lv)){
            stop("'lv' must be a single numeric position or a single character name.")
        }
        lv <- as.integer(lv)
        if(!length(levs)){
            stop("No grouping levels were detected in 'rd'.")
        }
        if(lv < 1L || lv > length(levs)){
            stop("'lv' is out of bounds for the detected grouping levels: ",
                 paste(levs, collapse = ", "))
        }
        levs <- levs[lv:length(levs)]
    } else {
        stop("'lv' must be numeric or character.")
    }

    if(!length(levs)){
        stop("No grouping levels remain after applying 'lv'.")
    }
    split_vars <- levs

    dotargs <- list(...)
    MoreArgs <- dotargs[["MoreArgs"]]
    if(!is.null(MoreArgs) && !is.list(MoreArgs)){
        stop("'MoreArgs' must be a list when supplied.")
    }
    dotargs[["MoreArgs"]] <- NULL

    if(exists("slitFrame", mode = "function")){
        cl1 <- slitFrame(rd, levs)
    } else {
        stop("Function 'slitFrame' was not found.")
    }

    nblocks <- length(cl1)
    if(!nblocks){
        .ringApply_signal("No split blocks were produced by slitFrame().")
        out <- rd[FALSE, , drop = FALSE]
        meta <- .biodry_get_meta(rd)
        attr(out, "BIOdry_meta") <- meta
        return(out)
    }

    dlen <- vapply(dotargs, function(z){
        if(is.null(z)) return(0L)
        if(is.atomic(z) || is.list(z)) length(z) else 1L
    }, integer(1L))

    badlen <- names(dlen)[!(dlen %in% c(0L, 1L, nblocks))]
    if(length(badlen)){
        .ringApply_signal(paste0(
            "Some arguments in '...' have lengths incompatible with the number of split blocks (",
            nblocks, "): ", paste(badlen, collapse = ", "),
            ". They will be recycled by base extraction rules."
        ))
    }

    ## a named vector whose names match the split labels (e.g. sc.c = Pradii03,
    ## named plot.tree) is given to each block by name through levexp(), not
    ## by position; other arguments are recycled over blocks as before. If
    ## the names match some blocks but not all, a warning names the argument
    ## and the unmatched blocks, and the values are given by position.
    dotargs_given <- dotargs
    arg_labels <- names(dotargs)
    if(is.null(arg_labels)) arg_labels <- rep("", length(dotargs))
    unnamed <- is.na(arg_labels) | !nzchar(arg_labels)
    arg_labels[unnamed] <- paste("argument", which(unnamed))
    dotargs <- Map(function(z, arg){
        if(!is.atomic(z) || length(z) <= 1L || is.null(names(z))) return(z)
        ex <- tryCatch(levexp(z, names(cl1)), error = function(e) NULL)
        if(is.null(ex) || !length(ex)) return(z)
        miss <- setdiff(names(cl1), names(ex))
        if(!length(miss)) return(ex[names(cl1)])
        warning(sprintf(paste0("The names of '%s' do not match every block; ",
                               "unmatched blocks: %s. Its values are given ",
                               "to blocks by position."),
                        arg, paste(miss, collapse = ", ")), call. = FALSE)
        z
    }, dotargs, arg_labels)
    names(dotargs) <- names(dotargs_given)

    fam <- get(fn, mode = "function")
    cl2 <- vector("list", nblocks)
    names(cl2) <- names(cl1)
    hy_start <- character(0)

    for(i in seq_len(nblocks)){
        iter_args <- lapply(dotargs, function(z){
            if(is.null(z)) return(NULL)
            if(length(z) <= 1L) return(z)
            z[[ ((i - 1L) %% length(z)) + 1L ]]
        })
        block_i <- cl1[[i]]
        block_i <- order_block_time(block_i, split_vars = split_vars)
        focus_i <- .biodry_resolve_focus(focus, block_i, old_meta, fn)
        dispatch_i <- .biodry_dispatch_mode(focus_i, focus_mode)

        if(identical(dispatch_i, "data.frame")) {
            block_arg <- if(identical(focus_i, ".data")) block_i else block_i[, focus_i, drop = FALSE]
            call_args <- c(list(block_arg), MoreArgs, iter_args)
        } else if(identical(dispatch_i, "vector")) {
            call_args <- c(list(block_i[[focus_i[1L]]]), MoreArgs, iter_args)
        } else if(identical(dispatch_i, "args")) {
            call_args <- c(unname(as.list(block_i[, focus_i, drop = FALSE])), MoreArgs, iter_args)
        } else {
            call_args <- c(list(block_i), MoreArgs, iter_args)
        }

        cl2[[i]] <- tryCatch({
            res_i <- do.call(fam, call_args)
            if(!is.null(attr(res_i, 'hydro_year_id')))
                hy_start <- c(hy_start, attr(res_i, 'hydro_year_start_month'))
            .biodry_finalize_result(
                block_i = block_i,
                res = res_i,
                focus_names = focus_i,
                target_name = if(is.null(target)) NULL else as.character(target)[1L]
            )},
            error = function(e){
                msg <- paste0("Function '", fn, "' failed on split block '",
                              names(cl1)[i], "': ", conditionMessage(e))
                if(identical(validate[1L], "none")) stop(msg, call. = FALSE)
                .ringApply_signal(msg)
                return(NULL)
            }
        )
    }

    keep <- !vapply(cl2, is.null, logical(1L))
    if(!all(keep)){
        .ringApply_signal(paste0(sum(!keep), " split block(s) returned NULL due to errors and were dropped."))
        cl2 <- cl2[keep]
    }
    if(!length(cl2)){
        stop("No successful results were returned by '", fn, "'.")
    }

    nord <- names(cl2)[order(names(cl2))]
    cl3 <- cl2[nord]

    cl4 <- tryCatch(
        do.call(rbind, cl3),
        error = function(e){
            stop("Could not row-bind results returned by '", fn,
                 "'. Ensure each split block returns compatible tabular outputs. ",
                 conditionMessage(e), call. = FALSE)
        }
    )
    rownames(cl4) <- NULL

    dnm <- duplicated(names(cl4)) | duplicated(names(cl4), fromLast = TRUE)
    if(any(dnm)){
        nms4 <- names(cl4)
        keep <- rep(TRUE, length(nms4))
        for(nm in unique(nms4[dnm])){
            idx <- which(nms4 == nm)
            base_col <- cl4[[idx[1L]]]
            same_as_first <- vapply(idx[-1L], function(j){
                isTRUE(all.equal(base_col, cl4[[j]], check.attributes = FALSE))
            }, logical(1L))
            if(all(same_as_first)){
                keep[idx[-1L]] <- FALSE
                .ringApply_signal(paste0(
                    "Dropped duplicated output column(s) named '", nm,
                    "' because they were exact duplicates after applying '", fn, "'."
                ))
            } else {
                names(cl4)[idx] <- make.unique(rep(nm, length(idx)))
                .ringApply_signal(paste0(
                    "Renamed non-identical duplicated output columns derived from '", nm,
                    "' after applying '", fn, "'."
                ))
            }
        }
        cl4 <- cl4[, keep, drop = FALSE]
    }

    cl4 <- order_output_columns(cl4, split_vars = split_vars, all_group_vars = all_group_vars)

    new_map <- .biodry_name_map(rd, cl4, fn)
    role_hints <- .biodry_role_hints(cl4, split_vars = split_vars)
    step_info <- list(
        fn = fn,
        lv_input = lv_input,
        lv_resolved = if(is.character(lv_input)) split_vars[1L] else split_vars[1L],
        split_vars = split_vars,
        output_names = names(cl4),
        arg_summary = c(.biodry_arg_summary(dotargs_given, MoreArgs),
                        list(focus = .biodry_compact_arg(focus),
                             target = .biodry_compact_arg(target),
                             focus_mode = focus_mode))
    )

    hist <- old_meta$history
    if(is.null(hist) || !is.list(hist)) hist <- list()
    hist[[length(hist) + 1L]] <- step_info

    meta <- old_meta
    meta$history <- hist
    meta$last_step <- step_info
    hy_start <- unique(hy_start)
    if(length(hy_start) == 1L) meta$hydro_year_start_month <- hy_start
    meta$name_map <- .biodry_merge_name_map(old_meta$name_map, new_map)
    meta$role_hints <- role_hints
    meta$split_vars <- split_vars
    meta$last_fn <- fn
    meta$meta_version <- "ringApply-attribute-v3"

    nm_map <- .biodry_map_to_named_character(meta$name_map)

    time_vars_old <- meta$time_vars
    if(is.null(time_vars_old)){
        if(!is.null(meta$time) && is.character(meta$time) && length(meta$time) == 1L) {
            time_vars_old <- meta$time
        } else {
            time_vars_old <- character(0L)
        }
    }
    if(length(time_vars_old)){
        time_vars_new <- unique(vapply(time_vars_old, function(tt){
            if(tt %in% names(nm_map)) unname(nm_map[[tt]]) else tt
        }, character(1L)))
        time_vars_new <- intersect(time_vars_new, names(cl4))
    } else {
        time_vars_new <- character(0L)
    }
    if(!length(time_vars_new) && !is.null(role_hints$time) && role_hints$time %in% names(cl4)){
        time_vars_new <- role_hints$time
    }
    meta$time_vars <- time_vars_new
    if(length(time_vars_new)){
        time_pref <- c("hydro_year_id", "year", "time", "month", "day")
        pick <- time_pref[time_pref %in% time_vars_new]
        meta$time <- if(length(pick)) pick[1L] else utils::tail(time_vars_new, 1L)
    } else if(!is.null(role_hints$time) && nzchar(role_hints$time)) {
        meta$time <- role_hints$time
    }

    groups_old <- meta$groups
    if(is.null(groups_old)) groups_old <- character(0L)
    groups_old_explicit <- length(groups_old) > 0L
    if(length(groups_old)){
        groups_new <- unique(vapply(groups_old, function(gg){
            if(gg %in% names(nm_map)) unname(nm_map[[gg]]) else gg
        }, character(1L)))
        groups_new <- intersect(groups_new, names(cl4))
    } else {
        groups_new <- character(0L)
    }

    time_in_groups_old <- isTRUE(meta$time_in_groups)
    if(groups_old_explicit){
        groups_final <- groups_new
    } else {
        groups_final <- intersect(role_hints$groups, names(cl4))
    }

    if(length(meta$time_vars) && !time_in_groups_old){
        groups_final <- setdiff(groups_final, meta$time_vars)
    }

    if(!groups_old_explicit && length(split_vars)){
        split_non_time <- setdiff(split_vars, c(meta$time_vars, .BIOdry_time_cols(cl4)))
        groups_final <- unique(c(split_non_time, groups_final))
    }

    groups_final <- unique(groups_final)
    meta$groups <- groups_final
    meta$time_in_groups <- isTRUE(length(meta$time_vars) && any(meta$time_vars %in% groups_final) && time_in_groups_old)

    if(identical(fn, "wlai") && "AI" %in% names(cl4)){
        resp_old <- meta$response
        if(is.null(resp_old)) resp_old <- character(0L)
        meta$response <- unique(c("AI", resp_old))
        meta$active_response <- "AI"
    } else if(is.null(meta$active_response) && !is.null(role_hints$response) && nzchar(role_hints$response)) {
        meta$active_response <- role_hints$response
        if(is.null(meta$response) || !length(meta$response)) meta$response <- role_hints$response
    }

    attr(cl4, "BIOdry_meta") <- meta
    return(cl4)
### \code{data.frame} object preserving initial factor-level columns.
} ,
ex=function() {

    ##Multilevel ecological data series (MEDS) of tree-ring widths:
    data(Prings05,envir = environment())
    ## Radial increments measured on 2003:
    data(Pradii03,envir = environment())    
    ## MEDS of monthly precipitation sums and average temperatures:
    data(PTclim05,envir = environment())

    ##Tree-level scaling of years of formation
    ##with 'rtimes' function:
    dfm1 <- ringApply(Prings05,
                      lv = 2,
                      fn = 'rtimes')
    str(dfm1)
    ##Relative time-units from year 1 to year 9:
    subset(dfm1,time%in%c(1:9,NA))

    ## Sample-level scaling of TRW chronologies around reference radii
    ## which were measured at 2003:
    dfm2 <- ringApply(dfm1,
                      lv = 'sample',
                      sc.c = Pradii03,
                      rf.t = 2003,
                      fn = 'scacum')
    str(dfm2)    
    ##Sample-level modeling of basal areas (mm2) via allometric
    ##scaling:
    dfm3 <- ringApply(dfm2,
                      lv = 'sample',
                      fn = 'amod',
                      MoreArgs = list(mp = c(2,1,0.25 * pi,2)))
    str(dfm3)

    ## Seasonal years from 'October' to 'September':
    cl1 <- ringApply(PTclim05,
                     lv = 'year',
                     fn = 'moveYr')
    tail(cl1,15)
    attr(cl1, 'BIOdry_meta')

    ##Year-level aridity indexes: 
    wl <- ringApply(cl1,
                    lv = 'year',
                    fn = 'wlai')
    str(wl)
    attr(wl, 'BIOdry_meta')

    ## Sample-level unit conversion on the active response:
    xu <- ringApply(Prings05,
                    lv = 'sample',
                    fn = 'mUnits',
                    from = 'mm',
                    to = 'cm')
    head(xu)

    ## Plot of aridity-index fluctuations:
    d <- groupedData(lmeForm(wl),wl)
    plot(d)        

})
