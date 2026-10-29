normalizeGroupLevels <- structure(function # Order grouping levels
### Grouping columns of processed data or of fitted objects are
### converted to factors whose levels follow numeric order (7, 17, 18,
### 56) instead of the order of first appearance, or alphabetical order
### for labels that are not numbers.
                        ##details<<The grouping columns are those named
                        ##in the second argument or, if it is not given,
                        ##those recorded in the \code{BIOdry_meta}
                        ##attribute, as set by
                        ##\code{\link{defineDataRoles}} or
                        ##\code{\link{ringApply}}. Fitted objects, such as
                        ##those from \code{\link{frametoLme}} or
                        ##\code{\link{modelFrame}}, carry no such metadata
                        ##themselves: the grouping columns are then read
                        ##from the metadata of their residual series
                        ##(\code{$fluc}), else from the grouping formula of
                        ##the fitted model, and only \code{$fluc} is
                        ##ordered; the model itself is unchanged. The
                        ##active temporal column is not treated as a
                        ##grouping column unless requested.
                        ##
                        ##A column is ordered as numbers only when all its
                        ##non-missing labels read as numbers; its levels
                        ##are then the labels themselves, ordered by their
                        ##value, so labels such as "07" are kept. Other
                        ##columns keep their labels, in alphabetical order.
                        ##Numeric columns become factors with sorted
                        ##levels. Values and rows are not changed. Ordered
                        ##levels give a natural panel order in trellis
                        ##plots.
(
    x, ##<<A \code{data.frame}, such as the output of
       ##\code{\link{ringApply}}, or a fitted object with a
       ##\code{$fluc} residual series, such as those from
       ##\code{\link{frametoLme}}, \code{\link{modelFrame}} or
       ##\code{\link{frametoFriedman}}.
    groups = NULL, ##<<\code{character} or \code{NULL}. Grouping
                   ##columns to order, as a vector such as
                   ##\code{c('plot', 'tree')} or a nested string such as
                   ##\code{'plot/tree/sample'}. If \code{NULL}, the
                   ##columns are read from the metadata.
    time = NULL, ##<<\code{character} or \code{NULL}. The active
                 ##temporal column. If \code{NULL}, it is read from the
                 ##metadata.
    keep_time_in_groups = NULL, ##<<\code{logical} or \code{NULL}. If
                                ##\code{TRUE}, the temporal column may stay
                                ##among the grouping columns; if
                                ##\code{FALSE}, it is removed. If
                                ##\code{NULL}, the metadata decide, and it
                                ##is removed by default.
    update_meta = TRUE, ##<<\code{logical}. Record the grouping
                        ##columns in the \code{BIOdry_meta} attribute of
                        ##the output.
    drop_unused = TRUE ##<<\code{logical}. Drop unused factor levels.
) {

  .parse_groups <- function(groups) {
    if(is.null(groups)) return(NULL)
    if(length(groups) == 1L && is.character(groups) && grepl("/", groups, fixed = TRUE)) {
      groups <- strsplit(groups, "/", fixed = TRUE)[[1L]]
    }
    groups <- as.character(groups)
    groups <- trimws(groups)
    groups[nzchar(groups)]
  }

  .get_meta <- function(obj) {
    attr(obj, "BIOdry_meta", exact = TRUE)
  }

  .set_meta <- function(obj, meta) {
    attr(obj, "BIOdry_meta") <- meta
    obj
  }

  .normalize_one <- function(v) {
    if(is.factor(v) || is.character(v)) {
      ch <- as.character(v)
      xn <- suppressWarnings(as.numeric(ch))
      ## numeric order only when every non-missing label reads as a
      ## number; the levels are the labels themselves (e.g. "07" stays
      ## "07"), ordered by their numeric value. Otherwise the labels are
      ## kept and ordered alphabetically
      if(any(!is.na(ch)) && !anyNA(xn[!is.na(ch)])) {
        lab <- unique(ch[!is.na(ch)])
        lev <- lab[order(suppressWarnings(as.numeric(lab)))]
        out <- factor(ch, levels = lev)
      } else {
        lev <- sort(unique(ch[!is.na(ch)]))
        out <- factor(ch, levels = lev)
      }
      return(out)
    }

    if(is.numeric(v) || is.integer(v)) {
      lev <- sort(unique(v[!is.na(v)]))
      return(factor(v, levels = lev))
    }

    v
  }

  .drop_levels <- function(df) {
    if(!is.data.frame(df)) return(df)
    for(nm in names(df)) {
      if(is.factor(df[[nm]])) df[[nm]] <- droplevels(df[[nm]])
    }
    df
  }

  .normalize_df <- function(df, group_vars) {
    group_vars <- intersect(group_vars, names(df))
    if(!length(group_vars)) return(df)

    for(g in group_vars) {
      df[[g]] <- .normalize_one(df[[g]])
    }

    if(isTRUE(drop_unused)) {
      df <- .drop_levels(df)
    }

    df
  }

  .meta_groups <- function(meta) {
    if(is.null(meta)) return(NULL)
    g <- unique(c(meta$groups, meta$role_hints$groups))
    g[!is.na(g) & nzchar(g)]
  }

  meta <- .get_meta(x)
  meta_groups <- .meta_groups(meta)

  ## fitted objects (e.g. from frametoLme()) carry no grouping metadata
  ## themselves: take it from $fluc, else from the grouping formula of
  ## the stored lme model
  if(!length(meta_groups) && !is.data.frame(x) && is.list(x)) {
    fluc_meta <- if(is.data.frame(x$fluc)) .get_meta(x$fluc) else NULL
    meta_groups <- .meta_groups(fluc_meta)
    if(is.null(meta)) meta <- fluc_meta
    if(!length(meta_groups) && inherits(x$model, "lme")) {
      gf <- tryCatch(nlme::getGroupsFormula(x$model),
                     error = function(e) NULL)
      if(!is.null(gf)) meta_groups <- all.vars(gf)
    }
  }

  active_time <- if(!is.null(time)) as.character(time)[1L] else NULL
  if(is.null(active_time) && !is.null(meta$time) && length(meta$time)) {
    active_time <- as.character(meta$time)[1L]
  }

  group_vars <- .parse_groups(groups)
  if(is.null(group_vars) || !length(group_vars)) {
    group_vars <- meta_groups
  }
  group_vars <- unique(group_vars)
  group_vars <- group_vars[!is.na(group_vars) & nzchar(group_vars)]

  keep_time <- keep_time_in_groups
  if(is.null(keep_time)) {
    keep_time <- isTRUE(meta$time_in_groups)
  }

  if(length(active_time) && !isTRUE(keep_time)) {
    group_vars <- setdiff(group_vars, active_time)
  }

  if(is.data.frame(x)) {
    out <- x
    out <- .normalize_df(out, group_vars)

    if(isTRUE(update_meta) && !is.null(meta)) {
      meta$groups <- unique(group_vars)
      if(length(active_time)) meta$time <- active_time
      if(!is.null(meta$role_hints)) {
        meta$role_hints$groups <- unique(group_vars)
        if(length(active_time)) meta$role_hints$time <- active_time
      }
      meta$time_in_groups <- isTRUE(keep_time)
      out <- .set_meta(out, meta)
    }
    return(out)
  }

  out <- x

  if(is.list(out) && !is.null(out$fluc) && is.data.frame(out$fluc)) {
    fluc_meta <- .get_meta(out$fluc)
    if(is.null(meta) && !is.null(fluc_meta)) meta <- fluc_meta

    out$fluc <- .normalize_df(out$fluc, group_vars)

    if(isTRUE(update_meta) && !is.null(meta)) {
      meta$groups <- unique(group_vars)
      if(length(active_time)) meta$time <- active_time
      if(!is.null(meta$role_hints)) {
        meta$role_hints$groups <- unique(group_vars)
        if(length(active_time)) meta$role_hints$time <- active_time
      }
      meta$time_in_groups <- isTRUE(keep_time)
      attr(out$fluc, "BIOdry_meta") <- meta
      attr(out, "BIOdry_meta") <- meta
    }
    return(out)
  }

  warning("No data-frame-like component found for grouping normalization; returning input unchanged.")
  out
### An object of the same kind as the input: a \code{data.frame} with
### ordered grouping columns and its attributes kept, or the fitted
### object with an ordered \code{$fluc}. If no data frame is found, the
### input is returned unchanged with a warning.
}, ex = function() {
    ## tree-ring widths (Prings05) and radii measured in 2003 (Pradii03);
    ## Zeide model with ln(time) and a tree-level random part, as in the
    ## frametoLme example
    data(Prings05, envir = environment())
    data(Pradii03, envir = environment())
    fit <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        ringApply(lv = 'sample', sc.c = Pradii03, rf.t = 2003,
                  fn = 'scacum') |>
        frametoLme(form = 'log(x) ~ log(csx) + log(time) | tree',
                   random = pdDiag(~ log(csx)))
    ## tree levels of the residual series, as nlme orders them
    levels(fit$fluc$tree)
    ## in numeric order
    fit_n <- normalizeGroupLevels(fit)
    levels(fit_n$fluc$tree)
    ## a data frame, with the grouping columns named
    dfm <- Prings05 |>
        ringApply(lv = 2, fn = 'rtimes') |>
        normalizeGroupLevels(groups = 'plot/tree/sample')
    lapply(dfm[c('plot', 'tree', 'sample')], levels)
})
