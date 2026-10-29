arguSelect <- structure(function#Argument selection
###Arguments of specific functions are selected from arbitrary numbers
###and varieties of arguments.
                     ##details<<Closures with ellipsis terms use this
                     ## function to extract and pass arguments to
                     ## other functions. Arguments in \code{MoreArgs}
                     ## lists are also extracted and stored again as
                     ## \code{MoreArgs} lists.
(
    rd = NULL, ##<<\code{NULL} or \code{data.frame}. Multilevel
               ##ecological data series. If \code{NULL} then this
               ##argument is ignored.
    fun = c('mapply','ringApply'), ##<< \code{character} or
                                   ##\code{NULL}. Vector of function
                                   ##names.
    ..., ##<< Further arguments not necessarily contained in the
        ##processed function(s).
    validate = c('none','warn','stop')
) {
    validate <- match.arg(validate)
    signal <- function(msg){
        if(validate == 'warn') warning(msg, call. = FALSE)
        if(validate == 'stop') stop(msg, call. = FALSE)
    }

    mx <- list(...)
    mar <- 'MoreArgs'
    fn <- mx[['fn']]
    fun <- unique(c(fun, fn))
    fun <- fun[!is.na(fun) & nzchar(fun)]

    missing_fun <- fun[!vapply(fun, exists, logical(1L), mode = 'function', inherits = TRUE)]
    if(length(missing_fun)) {
        signal(paste("Unknown function(s):", paste(missing_fun, collapse = ', ')))
        fun <- setdiff(fun, missing_fun)
    }

    if('ref' %in% names(mx)) {
        if(!is.data.frame(rd)) {
            signal("ref was supplied but rd is not a data.frame; ref cannot be expanded safely")
        } else {
            nl <- tryCatch(names(slitFrame(rd)), error = function(e) NULL)
            if(is.null(nl)) {
                signal("Could not derive multilevel names from rd while expanding ref")
            } else {
                refs <- tryCatch(levexp(mx[['ref']], nl), error = function(e) e)
                if(inherits(refs, 'error')) {
                    signal(paste("Could not expand ref:", conditionMessage(refs)))
                } else {
                    miss <- setdiff(nl, names(refs))
                    if(length(miss) && validate != 'none')
                        signal(paste("Expanded ref did not cover:", paste(miss, collapse = ', ')))
                    mx[['ref']] <- refs[nl]
                }
            }
        }
    }

    fca <- lapply(fun, function(x) names(formals(x)))
    nfr <- unique(unlist(fca, use.names = FALSE))
    mx. <- mx[!names(mx) %in% mar]
    kept_direct <- mx.[names(mx.) %in% nfr]
    dropped_direct <- setdiff(names(mx.), c(names(kept_direct), 'fn'))
    if(length(dropped_direct) && validate != 'none')
        signal(paste("Dropped direct arguments:", paste(dropped_direct, collapse = ', ')))

    sel <- kept_direct
    if(mar %in% names(mx)) {
        ma <- mx[[mar]]
        if(!is.list(ma)) {
            signal("MoreArgs must be a list")
        } else {
            man <- names(ma)
            if(is.null(man) && validate != 'none') {
                signal("Unnamed MoreArgs cannot be matched safely")
            } else {
                s <- man %in% nfr
                if(any(s)) sel[[mar]] <- ma[s]
                dropped_more <- man[!s]
                if(length(dropped_more) && validate != 'none')
                    signal(paste("Dropped MoreArgs entries:", paste(dropped_more, collapse = ', ')))
            }
        }
    }
    if(is.data.frame(rd)) sel[['rd']] <- rd
    sel
### \code{list} of selected arguments.
} , ex=function() {

    ##Multilevel ecological data series of tree-ring widths:
    data(Prings05,envir = environment())
    ## Radial increments measured on 2003:
    data(Pradii03,envir = environment())

    ## Selection of arguments in some functions:
    ar1 <- arguSelect(fun = c('amod'),
                      only.dup = TRUE,
                      mp = c(0.5,1),
                      rf.t = 2003)
    str(ar1)

    ar2 <- arguSelect(fn = 'amod',
                      only.dup = TRUE,
                      mp = c(0.5,1),
                      rf.t = 2003)
    str(ar2)
    ar3 <- arguSelect(rd = Prings05,
                      fn = 'amod',
                      only.dup = TRUE,
                      mp = c(0.5,1),
                      rf.t = 2003)
    str(ar3)

    ar4 <- arguSelect(rd = Prings05,
                      fun = 'scacum',
                      sc.c = Pradii03,
                      MoreArgs = list(only.dup = TRUE,
                                      mp = c(0.5,1),
                                      rf.t = 2003))
    str(ar4)

    ar5 <- arguSelect(rd = Prings05,
                      fun = 'scacum',
                      ref = Pradii03,
                      rf.t = rep(2003:2011),
                      MoreArgs = list(only.dup = TRUE,
                                      mp = c(0.5,1)))
    str(ar5)

})
