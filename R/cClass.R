cClass <- structure(function# Column-class extraction
### Column names of multilevel data sets are extracted according to
### three classes, which BIOdry uses as column roles: \code{numeric}
### (numeric columns that are not whole numbers; responses),
### \code{integer} (whole-number numeric columns, whatever their
### storage mode; time candidates), and \code{factor} (factor and
### character columns; grouping candidates, except the month column).
(
    rd, ##<<\code{data.frame}. Multilevel  data series.
    cl = 'all', ##<<\code{character} or \code{NULL}. Character vector
                  ##of classes to be considered. These can be
                  ##'numeric', 'integer', or 'factor'. If \code{'all'}
                  ##then all column names of \code{rd} are extracted.
    validate = c('none','warn','stop') ##<< Validation mode.
) {
    validate <- match.arg(validate)
    signal <- function(msg){
        if(validate == 'warn') warning(msg, call. = FALSE)
        if(validate == 'stop') stop(msg, call. = FALSE)
    }

    if(!is.data.frame(rd)) {
        signal("rd must be a data.frame")
        rd <- as.data.frame(rd)
    }
    if(is.null(names(rd)) || anyNA(names(rd)) || any(names(rd) == ''))
        signal("rd should have non-empty column names")

    if(is.null(cl) || identical(cl, 'all')) cl <- 'all'
    if(!is.character(cl)) {
        signal("cl must be a character vector or 'all'")
        cl <- 'all'
    }

    nms <- names(rd)
    is_num <- vapply(rd, is.numeric, logical(1L))
    num_names <- nms[is_num]

    integer_like <- num_names[vapply(rd[num_names], .BIOdry_is_whole, logical(1L))]
    numeric_like <- setdiff(num_names, integer_like)

    factor_like <- nms[vapply(rd, function(x)
        is.factor(x) || is.character(x), logical(1L))]

    cls <- list(numeric = numeric_like,
                integer = integer_like,
                factor = factor_like)
    if(cl[1L] %in% 'all') cl <- names(cls)
    bad <- setdiff(cl, names(cls))
    if(length(bad) > 0L) {
        signal(paste("Unknown class selector(s):", paste(bad, collapse = ', ')))
        cl <- intersect(cl, names(cls))
    }
    unlist(cls[cl], use.names = FALSE)
### \code{character} names.

} , ex=function() {
    ##Multilevel data frame of tree-ring widths:
    data(Prings05,envir = environment())
    ## Names of variables in Prings05 data containing numeric classes:
    cClass(Prings05, 'numeric') # 'x'
    ## Names of variables containing time units:
    cClass(Prings05, 'integer') # 'year'
    ## Names of variables containing factors:
    cClass(Prings05, 'factor') # 'sample', 'tree', 'plot'

})
