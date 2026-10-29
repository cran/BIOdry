amod <- structure(function
### Allometric scaling.
### Allometric models and parameters are used to scale organic growth.
                         ##details<<. This patched version keeps the
                         ##original block-level helper behavior, but it
                         ##also works on multilevel data frames. When a
                         ##data frame is supplied with \code{lv}, the
                         ##function first resolves the grouping
                         ##hierarchy from \code{lv}, then uses
                         ##\code{slitFrame()} to materialize those
                         ##groups, orders rows within each block on the
                         ##available time variables that remain after
                         ##splitting, and finally applies the
                         ##allometric model to each ordered block.
                         ##Character and numeric \code{lv} values are
                         ##both supported. Numeric \code{lv} values are
                         ##interpreted as positions in the detected
                         ##nesting structure, whereas character values
                         ##are matched to grouping names directly.
                         ##Cumulative organic growth is taken from
                         ##\code{csx} when available. If \code{csx} is
                         ##absent but \code{x} or \code{trw} is
                         ##present, cumulative growth is derived with
                         ##\code{cumsum()} after time ordering only for
                         ##the internal scaling step, so the function
                         ##can operate on simpler long-format tree-ring
                         ##tables. When the allometric model is applied,
                         ##the returned object always contains both the
                         ##scaled increments (\code{x}) and the scaled
                         ##cumulative growth state (\code{csx}). This
                         ##keeps the transformed increment and state
                         ##variables paired explicitly after scaling.
                         ##The first transformed increment is the change
                         ##from the size before the first ring (\code{csx}
                         ##minus the first ring width, or zero when no ring
                         ##widths are available) to the first transformed
                         ##cumulative value, so identity settings such as
                         ##\code{mp = c(1,1)} preserve the original
                         ##increments rather than introducing an initial
                         ##\code{NA}. Direct use
                         ##on climatic tables is only appropriate when a
                         ##meaningful cumulative source is available or
                         ##can be derived before scaling. The returned
                         ##data frame preserves the original row order
                         ##after block-wise processing and keeps scaled
                         ##outcomes before time and grouping columns.
                         (
                             x, ##<<\code{numeric} vector or
                                ##<<\code{data.frame}.
                             mp = c(1,1), ##<<\code{numeric}. Allometric
                                          ##parameters. Several groups
                                          ##may be supplied recursively.
                             fun = y ~ a*(x ^ b), ##<<\code{formula}.
                             lv = NULL, ##<<\code{NULL}, \code{character},
                                       ##<<or \code{numeric}. Grouping
                                       ##level(s) to split multilevel
                                       ##data frames before scaling.
                             validate = c('none','warn','stop') ##<<Validation mode.
                         ) {
                             validate <- match.arg(validate)
                             .signal <- function(msg){
                                 if (identical(validate, 'stop')) stop(msg, call. = FALSE)
                                 if (identical(validate, 'warn')) warning(msg, call. = FALSE)
                                 invisible(NULL)
                             }
                             .month_to_num <- function(v){
                                 if (is.factor(v)) v <- as.character(v)
                                 if (is.numeric(v)) {
                                     vv <- as.integer(v)
                                     if (all(is.na(vv) | (vv >= 1L & vv <= 12L))) return(vv)
                                 }
                                 if (is.character(v)) {
                                     s <- trimws(tolower(v))
                                     s3 <- substr(s, 1L, 3L)
                                     ab <- tolower(month.abb)
                                     nm <- tolower(month.name)
                                     out <- match(s3, ab)
                                     miss <- is.na(out)
                                     if (any(miss)) out[miss] <- match(s[miss], nm)
                                     return(as.integer(out))
                                 }
                                 rep.int(NA_integer_, length(v))
                             }
                             .time_candidates <- function(df){
                                 nms <- names(df)
                                 low <- tolower(nms)
                                 out <- character(0)
                                 add <- function(nm) if (nm %in% nms && !nm %in% out) out <<- c(out, nm)
                                 for (nm in c('hydro_year_id','year','yr','calendar_year','hydro_year')) add(nm)
                                 # month-like columns by name or values
                                 for (i in seq_along(nms)) {
                                     nm <- nms[i]
                                     ln <- low[i]
                                     v <- df[[i]]
                                     if (ln %in% c('month','mon','mnt') || any(grepl('month', ln, fixed = TRUE))) {
                                         if (all(is.na(.month_to_num(v)) | (.month_to_num(v) >= 1L & .month_to_num(v) <= 12L))) add(nm)
                                     }
                                 }
                                 for (nm in c('week','wk','day','doy','hour','minute','time','rel_time','ring_time','cambial_age','age')) add(nm)
                                 out
                             }
                             .group_candidates <- function(df){
                                 ## factor and character columns that are not months (see cClass)
                                 .BIOdry_group_cols(df)
                             }
                             .resolve_lv <- function(df, lv){
                                 levs <- .group_candidates(df)
                                 if (is.null(lv)) return(list(split_vars = character(0), hierarchy = levs))
                                 if (is.numeric(lv)) {
                                     if (length(lv) != 1L || is.na(lv) || lv < 1L || lv > length(levs)) {
                                         stop("'lv' is out of bounds for the detected grouping hierarchy.", call. = FALSE)
                                     }
                                     pos <- as.integer(lv)
                                     return(list(split_vars = levs[pos:length(levs)], hierarchy = levs))
                                 }
                                 if (is.character(lv)) {
                                     if (!all(lv %in% names(df))) stop("All character values in 'lv' must be column names.", call. = FALSE)
                                     if (length(lv) == 1L) {
                                         if (!lv %in% levs) levs <- c(lv, levs)
                                         pos <- match(lv, levs)
                                         return(list(split_vars = levs[pos:length(levs)], hierarchy = levs))
                                     }
                                     # explicit split vars, preserve user order where possible
                                     split_vars <- unique(lv)
                                     return(list(split_vars = split_vars, hierarchy = levs))
                                 }
                                 stop("'lv' must be NULL, character, or numeric.", call. = FALSE)
                             }
                             .order_block <- function(df, split_vars){
                                 tv <- .time_candidates(df)
                                 ord_vars <- setdiff(tv, split_vars)
                                 if (!length(ord_vars)) return(df)
                                 ord_list <- vector('list', length(ord_vars))
                                 names(ord_list) <- ord_vars
                                 for (i in seq_along(ord_vars)) {
                                     nm <- ord_vars[i]
                                     v <- df[[nm]]
                                     mv <- .month_to_num(v)
                                     if (all(!is.na(mv))) {
                                         ord_list[[i]] <- mv
                                     } else if (inherits(v, c('POSIXct','POSIXlt','Date'))) {
                                         ord_list[[i]] <- as.numeric(v)
                                     } else if (is.factor(v)) {
                                         ord_list[[i]] <- as.character(v)
                                     } else {
                                         ord_list[[i]] <- v
                                     }
                                 }
                                 o <- do.call(order, c(ord_list, list(na.last = TRUE, method = 'radix')))
                                 df[o, , drop = FALSE]
                             }
                             .rhs_text <- function(f) {
                                 if (inherits(f, 'formula')) paste(deparse(f[[3L]]), collapse = '') else as.character(f)
                             }
                             .param_names <- function(f){
                                 allv <- all.vars(f)
                                 setdiff(allv, letters[20:26])
                             }
                             .eval_fun <- function(rhs, x, params){
                                 e <- c(list(x = x, y = NULL), params)
                                 eval(parse(text = rhs), envir = e)
                             }
                             .as_mp_list <- function(mp, prm){
                                 if (is.list(mp)) return(mp)
                                 if (!length(prm)) return(list(mp))
                                 spt <- ceiling(seq_along(mp)/length(prm))
                                 split(mp, spt)
                             }
                             .choose_source <- function(df, split_vars){
                                 nms <- names(df)
                                 low <- tolower(nms)
                                 if ('csx' %in% nms) return(list(mode = 'csx', col = 'csx', return_csx = TRUE))
                                 if ('x' %in% nms) {
                                     .signal("'csx' not found; deriving cumulative growth from 'x'.")
                                     return(list(mode = 'increment', col = 'x', return_csx = TRUE))
                                 }
                                 if ('trw' %in% low) {
                                     col <- nms[match('trw', low)]
                                     .signal("'csx' not found; deriving cumulative growth from 'trw'.")
                                     return(list(mode = 'increment', col = col, return_csx = TRUE))
                                 }
                                 cand <- setdiff(.BIOdry_response_cols(df), split_vars)
                                 if (length(cand)) {
                                     .signal(sprintf("Using '%s' as cumulative growth source by fallback.", cand[1L]))
                                     return(list(mode = 'increment', col = cand[1L], return_csx = TRUE))
                                 }
                                 stop("Could not find a cumulative growth source. Expected 'csx', or increments in 'x'/'trw'.", call. = FALSE)
                             }
                             .apply_block <- function(df, split_vars = character(0)){
                                 df <- .order_block(df, split_vars)
                                 src <- .choose_source(df, split_vars)
                                 csx_in <- if (identical(src$mode, 'csx')) df[[src$col]] else cumsum(df[[src$col]])
                                 rhs <- .rhs_text(fun)
                                 prm <- .param_names(fun)
                                 mp_list <- .as_mp_list(mp, prm)
                                 if (length(prm)) {
                                     dpr <- data.frame(do.call(rbind, mp_list), check.names = FALSE)
                                     names(dpr) <- prm
                                 } else {
                                     dpr <- data.frame(dummy = seq_along(mp_list))[FALSE]
                                 }
                                 x_scaled <- csx_in
                                 ## size before the first ring: csx1 - x1 when the
                                 ## block has a cumulative state and its ring widths
                                 ## (e.g. csx scaled by scacum), otherwise 0
                                 prev <- if (identical(src$mode, 'csx') && 'x' %in% names(df)) csx_in[1L] - df[['x']][1L] else NA_real_
                                 for (i in seq_along(mp_list)) {
                                     params <- if (ncol(dpr)) as.list(dpr[i, , drop = FALSE]) else list()
                                     x_scaled <- .eval_fun(rhs, x_scaled, params)
                                     if (!is.na(prev)) prev <- .eval_fun(rhs, prev, params)
                                 }
                                 first <- if (is.na(prev)) x_scaled[1L] else x_scaled[1L] - prev
                                 x_inc <- c(first, diff(x_scaled))
                                 out <- data.frame(x = x_inc, csx = x_scaled, check.names = FALSE)
                                 keep_other <- setdiff(names(df), c('x','csx'))
                                 if (length(keep_other)) out <- cbind(out, df[, keep_other, drop = FALSE])
                                 out
                             }
                             .reorder_cols <- function(df, split_info){
                                 nms <- names(df)
                                 low <- tolower(nms)
                                 outcomes_pref <- c('x','csx','pr','pm','tm')
                                 outcomes <- outcomes_pref[outcomes_pref %in% low]
                                 outcomes <- nms[match(outcomes, low)]
                                 outcomes <- outcomes[!is.na(outcomes)]
                                 other_num <- nms[vapply(df, is.numeric, logical(1))]
                                 time_pref <- c('hour','day','week','month','year','hydro_year_id','yr','calendar_year','time','rel_time','ring_time','cambial_age','age')
                                 time_vars <- intersect(nms, .time_candidates(df))
                                 time_order <- nms[match(intersect(time_pref, low), low)]
                                 time_order <- unique(c(time_order, setdiff(time_vars, time_order)))
                                 group_vars <- split_info$split_vars
                                 all_groups <- split_info$hierarchy
                                 broader <- setdiff(all_groups, group_vars)
                                 group_order <- unique(c(group_vars, broader))
                                 group_order <- group_order[group_order %in% nms]
                                 num_first <- unique(c(outcomes, setdiff(other_num, c(outcomes, time_order, group_order))))
                                 ord <- unique(c(num_first, time_order, group_order, nms))
                                 df[, ord[ord %in% nms], drop = FALSE]
                             }
                             
                             if (!is.data.frame(x)) {
                                 rhs <- .rhs_text(fun)
                                 prm <- .param_names(fun)
                                 mp_list <- .as_mp_list(mp, prm)
                                 if (length(prm)) {
                                     dpr <- data.frame(do.call(rbind, mp_list), check.names = FALSE)
                                     names(dpr) <- prm
                                 } else {
                                     dpr <- data.frame(dummy = seq_along(mp_list))[FALSE]
                                 }
                                 x_scaled <- x
                                 for (i in seq_along(mp_list)) {
                                     params <- if (ncol(dpr)) as.list(dpr[i, , drop = FALSE]) else list()
                                     x_scaled <- .eval_fun(rhs, x_scaled, params)
                                 }
                                 x1 <- c(x_scaled[1L], diff(x_scaled))
                                 names(x1) <- names(x_scaled)
                                 xd <- data.frame(x = x1, csx = x_scaled, check.names = FALSE)
                                 return(xd)
                             }
                             
                             cd <- x
                             cd[['..rowid__']] <- seq_len(nrow(cd))
                             split_info <- .resolve_lv(cd, lv)
                             split_vars <- split_info$split_vars
                             if (!length(split_vars)) {
                                 xd <- .apply_block(cd, split_vars = character(0))
                                 xd <- xd[order(cd[['..rowid__']]), , drop = FALSE]
                             } else {
                                 key <- interaction(cd[, split_vars, drop = FALSE], drop = TRUE, lex.order = FALSE)
                                 idx <- split(seq_len(nrow(cd)), key, drop = TRUE)
                                 res <- lapply(idx, function(ii){
                                     blk <- cd[ii, , drop = FALSE]
                                     .apply_block(blk, split_vars = split_vars)
                                 })
                                 xd <- do.call(rbind, res)
                                 xd <- xd[order(xd[['..rowid__']]), , drop = FALSE]
                             }
                             xd[['..rowid__']] <- NULL
                             xd <- .reorder_cols(xd, split_info)
                             attr(xd, 'split_vars') <- split_vars
                             attr(xd, 'time_vars') <- .time_candidates(xd)
                             return(xd)
### \code{data.frame} of the scaled variable (x) and cumulative
### scaled growth (csx). For multilevel data frames, these are
### computed within each ordered split block.
                         } , ex=function() {
                             ## Simulating TRW records:
                             set.seed(1)
                             trw <- ts(abs(rnorm(12,1,1)),start = 1950)
                             ## Cumulative TRW:
                             cri <- cumsum(trw)
                             ## tree diameters
                             td <- amod(cri,mp = c(2,1))
                             ## plot of the tree diameters and the
                             ## relative increments:
                             plot(ts(td))
                         })
