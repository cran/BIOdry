tdForm <- structure(function#ltd formulas
### This function formulates linear time-decline formulas (ltd) from
### categorical variables in multilevel ecological data series.
                    ##details<< the ltd formulas belong to following
                    ##general equation: log (x) = log (csx) + f(time);
                    ##where the relative organic growth (x) is
                    ##explained by the cumulative organic growth (csx)
                    ##plus a function of time f(time); with f(time)
                    ##being either the time or a logarithmic
                    ##transformation the time. The ltd can be
                    ##implemented by \code{\link{modelFrame}} function
                    ##to subtract trends in organic MEDS.
                    ##
                    ##The response \code{x}, the cumulative growth
                    ##\code{csx} and the time covariate (\code{time}
                    ##from \code{\link{rtimes}}, or \code{year}) are the
                    ##package's fixed column names. The grouping levels
                    ##are the factor and character columns other than
                    ##the month column, outer level first (the reverse
                    ##of the column order; see \code{\link{cClass}}). A
                    ##time variable or grouping
                    ##levels declared with \code{\link{defineDataRoles}}
                    ##take precedence; the metadata that
                    ##\code{\link{ringApply}} records automatically does
                    ##not.

                    ##references<< Zeide B. 1993. Analysis of Growth
                    ##Equations. For. Sci., 39: 594-616.
(
    rd, ##<<\code{data.frame} or \code{character} vector. Multilevel
        ##ecological data series or vector of ecological factors.
    prim.cov = FALSE, ##<<\code{logical}. Print a primary covariate
                      ##form: \code{'~ cov'}. If FALSE then a complete
                      ##formula: \code{'resp ~ cov | group'} is printed.
    on.time = TRUE, ##<< \code{logical}. If TRUE (default) the relative
                    ##time column \code{'time'} from \code{\link{rtimes}}
                    ##is used when present, otherwise \code{'year'}.
                    ##If FALSE \code{'year'} is used when
                    ##present, otherwise \code{'time'}. A time variable
                    ##declared with \code{defineDataRoles(time = ...)}
                    ##takes precedence over both.
    log.t = FALSE, ##<< \code{logical}. If TRUE then \code{f(time) =
                   ##ln(time)}. Default FALSE produces a log-linear
                   ##time-decline formula. Both settings are Zeide forms,
                   ##f(time) = time (FALSE) and f(time) = ln(time) (TRUE),
                   ##with the same number of parameters; fit both by
                   ##maximum likelihood (\code{method = "ML"} in
                   ##\code{\link{frametoLme}}) and choose by AIC or BIC.
    lev.rm = NULL, ##<< NULL or \code{character} name of the ecological
                  ##factor(s) in the MEDS to be removed from the
                  ##formula.
    validate = c('none','warn','stop') ##<< \code{character}. Validation mode: \code{'none'} silences checks, \code{'warn'} emits warnings, and \code{'stop'} stops on invalid inputs.
) {
    validate <- match.arg(validate)

    signal <- function(msg){
        if(validate == 'warn') warning(msg, call. = FALSE)
        if(validate == 'stop') stop(msg, call. = FALSE)
    }

    .get_meta <- function(x){
        meta <- attr(x, 'BIOdry_meta', exact = TRUE)
        if(is.null(meta) || !is.list(meta)) meta <- list()
        meta
    }

    .split_groups <- function(x){
        if(is.null(x)) return(character(0L))
        if(is.numeric(x)) return(as.character(x))
        if(is.character(x) && length(x) == 1L && grepl('/', x, fixed = TRUE)) {
            x <- strsplit(x, '/', fixed = TRUE)[[1L]]
        }
        x <- as.character(x)
        x <- trimws(x)
        x[!is.na(x) & nzchar(x)]
    }

    .normalize_name_map <- function(x){
        if(is.null(x)) return(setNames(character(0L), character(0L)))
        if(is.list(x)) {
            if(!length(x)) return(setNames(character(0L), character(0L)))
            vals <- vapply(x, function(z){
                if(is.null(z) || !length(z) || is.na(z[1L])) return(NA_character_)
                as.character(z[1L])
            }, character(1L))
            nms <- names(x)
            keep <- !is.na(vals) & nzchar(vals) & !is.null(nms) & nzchar(nms)
            return(stats::setNames(vals[keep], nms[keep]))
        }
        if(is.character(x)) {
            nms <- names(x)
            if(is.null(nms)) return(setNames(character(0L), character(0L)))
            keep <- !is.na(x) & nzchar(x) & nzchar(nms)
            return(stats::setNames(unname(x[keep]), nms[keep]))
        }
        setNames(character(0L), character(0L))
    }

    .resolve_name <- function(nm, meta, nms){
        if(!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(nm)) return(NULL)
        if(nm %in% nms) return(nm)
        nm_map <- .normalize_name_map(meta$name_map)
        if(length(nm_map) && nm %in% names(nm_map)) {
            target <- unname(nm_map[[nm]])
            if(length(target) == 1L && !is.na(target) && nzchar(target) && target %in% nms) {
                return(target)
            }
        }
        NULL
    }

    .resolve_time_from_meta <- function(meta, nms){
        t_meta <- .resolve_name(meta$time, meta, nms)
        if(length(t_meta) == 1L && !is.null(t_meta)) return(t_meta)
        tv <- meta$time_vars
        if(is.null(tv)) tv <- character(0L)
        tv <- unique(unlist(lapply(tv, .resolve_name, meta = meta, nms = nms), use.names = FALSE))
        tv <- tv[!is.na(tv) & nzchar(tv)]
        if(!length(tv)) return(NULL)
        tv[1L]
    }

    .resolve_groups_from_meta <- function(meta, nms){
        g <- .split_groups(meta$groups)
        if(!length(g)) return(character(0L))
        out <- unlist(lapply(g, .resolve_name, meta = meta, nms = nms), use.names = FALSE)
        out <- out[!is.na(out) & nzchar(out)]
        unique(out)
    }

    if(length(prim.cov) != 1L || !is.logical(prim.cov) || is.na(prim.cov)) {
        signal('prim.cov must be TRUE or FALSE')
        prim.cov <- FALSE
    }
    if(length(on.time) != 1L || !is.logical(on.time) || is.na(on.time)) {
        signal('on.time must be TRUE or FALSE')
        on.time <- TRUE
    }
    if(length(log.t) != 1L || !is.logical(log.t) || is.na(log.t)) {
        signal('log.t must be TRUE or FALSE')
        log.t <- FALSE
    }

    rs <- 'log(x)'; lx <- '~ log(csx) +'
    t <- if(on.time) 'time' else 'year'

    if(!is.character(rd)){
        if(!is.data.frame(rd)) {
            if(validate == 'stop') stop('rd must be a data.frame or a character vector', call. = FALSE)
            rd <- as.data.frame(rd)
        }
        meta <- .get_meta(rd)
        nms <- names(rd)
        if(is.null(nms) || any(nms == ''))
            signal('rd should have non-empty column names')
        nml <- tolower(nms)
        trw_cols <- nms[nml %in% c('trw','x')]
        csx_cols <- nms[nml %in% 'csx']
        climate_like <- nms[nml %in% c('tm','pm','tmax','tmin','temp',
                                       'temperature','prec','precip',
                                       'precipitation','ppt','spei','spi',
                                       'pet','vpd','humidity','rh')]
        if(length(trw_cols) == 0L)
            signal("tdForm() expects tree-ring widths in 'trw' or prepared response 'x'")
        if(length(csx_cols) == 0L)
            signal("tdForm() did not find cumulative organic growth column 'csx'")
        if(length(trw_cols) == 0L && length(csx_cols) == 0L && length(climate_like) > 0L)
            signal('tdForm() input looks climatic; use lmeForm() or an external climate-model function')
        if(length(climate_like) > 0L && length(trw_cols) > 0L && length(csx_cols) > 0L && validate != 'none')
            signal('tdForm() detected climate-like columns but will proceed because tree-growth columns are present')

        ## the package's own column names: 'time' (from rtimes) and 'year'
        year_like <- intersect('year', nms)
        time_like <- intersect('time', nms)

        t_meta <- .resolve_time_from_meta(meta, nms)
        time_declared <- is.call(meta$call) && !is.null(as.list(meta$call)[['time']])
        if(time_declared && length(t_meta) == 1L && !is.null(t_meta)) {
            t <- t_meta
        } else if(on.time){
            if(length(time_like) > 0L) {
                t <- time_like[1L]
            } else if(length(year_like) > 0L) {
                t <- year_like[1L]
                if(validate != 'none')
                    signal(paste('No relative time column found; using', t, 'instead'))
            }
        } else {
            if(length(year_like) > 0L) {
                t <- year_like[1L]
            } else if(length(time_like) > 0L) {
                t <- time_like[1L]
                if(validate != 'none')
                    signal(paste('No year-like column found; using', t, 'instead'))
            }
        }

        mg <- if(.BIOdry_declared(meta, 'groups')) .resolve_groups_from_meta(meta, nms) else character(0L)
        if(is.numeric(lev.rm) && length(lev.rm)) {
            bad <- lev.rm[lev.rm < 1L | lev.rm > length(nms)]
            if(length(bad) && validate != 'none')
                signal('Some numeric lev.rm indices are out of range and will be ignored')
            lev.rm <- nms[lev.rm[lev.rm >= 1L & lev.rm <= length(nms)]]
        }
        if(length(mg)) {
            nf <- mg[!mg %in% lev.rm]
            nf <- unique(nf[nf %in% nms])
        } else {
            f <- .BIOdry_group_cols(rd)
            nf <- rev(f[!f %in% lev.rm])
            if(length(nf) == 0L && validate != 'none')
                signal('tdForm() detected no factor grouping columns')
        }
    } else {
        nf <- rd[!rd %in% lev.rm]
    }

    if(log.t) t <- paste('log(', t, ')', sep = '')
    ftt <- paste(rs, lx, t, sep = ' ')
    sep. <- if(length(nf) == 0L) '' else ' | '
    fc <- paste(nf, collapse = '/')
    fr <- paste(ftt, fc, sep = sep.)
    if(prim.cov) fr <- paste(lx, t, sep = '')
    formula(fr)
### \code{formula} with the forms: 'resp ~ cov | group' or '~ cov'.
} , ex=function(){
    ## an ltd formula:
    lev <- c('plot','tree')
    tdeq <- tdForm(lev,log.t = TRUE)
    tdeq
    ## (not run) only primary covariate:
    tdeq1 <- tdForm(lev,prim.cov = TRUE)
    tdeq1
    ##Multilevel data frame of tree-ring widths:
    data(Prings05,envir = environment())
    ## removing two levels: 'plot' and 'tree' from the formula
    tdea2 <- tdForm(Prings05, lev.rm = c('plot','tree'))
    tdea2 <- tdForm(Prings05, lev.rm = 2:3)
})
