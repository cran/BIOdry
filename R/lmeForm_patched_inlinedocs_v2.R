lmeForm <- structure(function#LME formula
### This function computes LME formulas from multilevel ecological
### data series (MEDS).
                     ##details<< Formulas of the form \code{resp ~ cov
                     ##| group} (see \code{\link[nlme]{groupedData}}
                     ##function) are computed from MEDS. The formulas
                     ##can be implemented by
                     ##\code{\link{modelFrame}} function to detrend MEDS.
                     ##
                     ##Column roles come from column types (see
                     ##\code{\link{cClass}}): the response is
                     ##the first non-integer numeric column, the
                     ##covariate is the first whole-number numeric
                     ##column, and the grouping levels are the factor
                     ##and character columns, outer level first (the
                     ##reverse of the column order). The month column
                     ##(named \code{month}, or a factor whose levels are
                     ##month names) is never a grouping level.
                     ##
                     ##A response, time covariate or grouping levels
                     ##declared with \code{\link{defineDataRoles}} take
                     ##precedence. The metadata that
                     ##\code{\link{ringApply}} records automatically does
                     ##not override the type rule. A declared time
                     ##variable is removed from the grouping levels
                     ##unless the metadata sets
                     ##\code{time_in_groups = TRUE}. To use a
                     ##whole-number column such as \code{year} as a
                     ##grouping level, see \code{\link{frametoLme}}.

                     ##references<< Pinheiro J. C.,
                     ##D. M. Bates. 2000. Mixed-effects models in S
                     ##and S-PLUS. Springer, New York.
(
    rd, ##<< \code{data.frame}. Multilevel ecological data series
    prim.cov = FALSE, ##<<\code{Logical}: should the LME formula only
                      ##be printed in primary covariate form: '~ cov'?
                      ##If FALSE then a complete form: 'resp ~ covar |
                      ##group' is formulated.
    resp = NULL, ##<<\code{NULL} or \code{character}. Column name of
                 ##the response. If NULL then a response declared with
                 ##\code{defineDataRoles} is used, and otherwise the
                 ##first non-integer numeric column of the MEDS.
    covar = NULL, ##<<\code{NULL} or \code{character}. Column name(s)
                  ##of the covariate(s). If \code{NULL} then a time
                  ##variable declared with \code{defineDataRoles} is
                  ##used, and otherwise the first whole-number numeric
                  ##column of the MEDS.
    lev.rm = NULL, ##<< \code{NULL}, \code{character} or \code{numeric}
                  ##vector of levels in the MEDS to be removed from
                  ##the groups.
    validate = c('none','warn','stop')

) {
    validate <- match.arg(validate)

    signal <- function(msg){
        if(validate == 'warn') warning(msg, call. = FALSE)
        if(validate == 'stop') stop(msg, call. = FALSE)
        invisible(NULL)
    }

    if(!is.data.frame(rd)) {
        signal("rd must be a data.frame")
        rd <- as.data.frame(rd)
    }
    if(length(prim.cov) != 1L || !is.logical(prim.cov) || is.na(prim.cov)) {
        signal("prim.cov must be TRUE or FALSE")
        prim.cov <- FALSE
    }

    meta <- attr(rd, "BIOdry_meta", exact = TRUE)
    if(is.null(meta) || !is.list(meta)) meta <- list()

    resolve_name <- function(x, meta, rd_names){
        if(is.null(x) || !length(x)) return(x)
        x <- as.character(x)
        nm_map <- meta$name_map
        if(is.null(nm_map) || !is.list(nm_map)) nm_map <- list()
        out <- vapply(x, function(one){
            if(is.na(one) || !nzchar(one)) return(one)
            if(one %in% rd_names) return(one)
            mapped <- nm_map[[one]]
            if(is.null(mapped) || !length(mapped)) return(one)
            mapped <- as.character(mapped)[1L]
            if(is.na(mapped) || !nzchar(mapped)) return(one)
            if(mapped %in% rd_names) return(mapped)
            one
        }, character(1L), USE.NAMES = FALSE)
        out
    }

    resolve_hint <- function(value, rd_names, meta){
        value <- resolve_name(value, meta = meta, rd_names = rd_names)
        value <- value[!is.na(value) & nzchar(value)]
        value[value %in% rd_names]
    }

    rd_names <- names(rd)
    meta_roles <- meta$role_hints
    if(is.null(meta_roles) || !is.list(meta_roles)) meta_roles <- list()

    if(is.null(resp)) {
        ## a response from metadata decides only when declared with
        ## defineDataRoles(response = ...)
        meta_resp <- if(.BIOdry_declared(meta, 'response'))
            resolve_hint(c(meta$active_response, meta_roles$response),
                         rd_names = rd_names, meta = meta) else character(0)
        if(length(meta_resp)) {
            resp <- meta_resp[1L]
        } else {
            numc <- cClass(rd, 'numeric', validate = 'none')
            if(length(numc) > 1L && validate != 'none')
                signal(paste("Multiple numeric response candidates detected:",
                             paste(numc, collapse = ', '),
                             "- using", numc[1L]))
            resp <- numc[1L]
        }
    } else {
        resp <- resolve_name(resp, meta = meta, rd_names = rd_names)
    }

    if(is.null(covar)) {
        ## a time variable from metadata decides only when the user declared it
        ## with defineDataRoles(time = ...); metadata that ringApply() creates
        ## automatically does not (as in BIOdry 0.9.1, the first integer
        ## time-like column is used then)
        time_declared <- is.call(meta$call) && !is.null(as.list(meta$call)[['time']])
        meta_covar <- if(time_declared) {
            resolve_hint(c(meta$time, meta$time_vars, meta_roles$time),
                         rd_names = rd_names, meta = meta)
        } else character(0)
        if(length(meta_covar)) {
            covar <- meta_covar[1L]
        } else {
            ## first whole-number column, as in BIOdry 0.9.1
            intc <- .BIOdry_time_cols(rd)
            covar <- intc[1L]
            if(length(intc) > 1L && validate != 'none')
                signal(paste("Multiple integer-like covariates detected:",
                             paste(intc, collapse = ', '),
                             "- using", covar))
        }
    } else {
        covar <- resolve_name(covar, meta = meta, rd_names = rd_names)
    }

    if((is.null(resp) || !length(resp) || is.na(resp)) && validate == 'stop')
        stop("Could not determine a response variable", call. = FALSE)
    if((is.null(covar) || !length(covar) || anyNA(covar)) && validate == 'stop')
        stop("Could not determine a covariate", call. = FALSE)

    covar <- as.character(covar)
    resp <- as.character(resp)[1L]

    covar. <- formula(paste('~', paste(covar, collapse = ' + '), sep = ' '), showEnv = FALSE)
    covar_full <- paste(resp, '~', paste(covar, collapse = ' + '), sep = ' ')

    f <- .BIOdry_group_cols(rd)

    ## grouping from metadata decides only when declared with
    ## defineDataRoles(groups = ...)
    meta_groups <- if(.BIOdry_declared(meta, 'groups'))
        unique(c(meta$groups, meta_roles$groups)) else character(0)
    meta_groups <- resolve_hint(meta_groups, rd_names = rd_names, meta = meta)

    active_time <- unique(resolve_hint(c(meta$time, meta$time_vars, meta_roles$time, covar),
                                       rd_names = rd_names, meta = meta))
    if(!isTRUE(meta$time_in_groups) && length(active_time)) {
        meta_groups <- setdiff(meta_groups, active_time)
    }

    meta_groups <- setdiff(meta_groups, c(resp, covar))

    if(length(meta_groups)) {
        gvars <- unique(c(intersect(f, meta_groups), meta_groups))
    } else {
        gvars <- f
    }

    if(is.numeric(lev.rm) && length(lev.rm)) {
        bad <- lev.rm[lev.rm < 1L | lev.rm > length(gvars)]
        if(length(bad) && validate != 'none')
            signal("Some numeric lev.rm indices are out of range and will be ignored")
        lev.rm <- gvars[lev.rm[lev.rm >= 1L & lev.rm <= length(gvars)]]
    }
    lev.rm <- resolve_name(lev.rm, meta = meta, rd_names = rd_names)

    nf <- rev(gvars[!gvars %in% lev.rm])
    sep. <- if(length(nf) == 0L) '' else ' | '
    fc <- paste(nf, collapse = '/')
    fr <- paste(covar_full, fc, sep = sep.)
    fr <- formula(fr, showEnv = FALSE)
    if(prim.cov) fr <- covar.

    fr
### \code{formula} with any of the forms: \code{resp ~ cov | group} or
### \code{~ cov}.
} , ex=function(){
    ##Multilevel ecological data series of tree-ring widths:
    data(Prings05,envir = environment())

    ## LME formula:
    form1 <- lmeForm(Prings05,prim.cov = FALSE)
    print(form1)
    ## removing the sample level from the formula
    form2 <- lmeForm(Prings05,lev.rm = 'sample')
    form2 <- lmeForm(Prings05,lev.rm = 1)

    ## groupedData object with the LME formula
    gdata <- groupedData(lmeForm(Prings05,lev.rm = 1),
                         data = Prings05)
    plot(gdata,groups = ~ sample)
})
