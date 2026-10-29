## Column roles from column types, as in BIOdry 0.9.1 (cClass): factor and
## character columns are groups, whole-number numeric columns are time
## candidates, and other numeric columns are responses. The month column
## (the package's 'month', or a factor whose levels are month names) is
## never a group.
## Roles declared with defineDataRoles() override these rules; metadata that
## ringApply() creates automatically does not.
.BIOdry_is_month_col <- function(z, nm = "") {
    if (identical(nm, "month")) return(TRUE)
    if (!is.factor(z) || !nlevels(z)) return(FALSE)
    all(levels(z) %in% c(month.abb, month.name))
}

.BIOdry_is_whole <- function(z) {
    is.numeric(z) && all(floor(z) == z, na.rm = TRUE)
}

.BIOdry_group_cols <- function(d) {
    nms <- names(d)
    if (is.null(nms)) return(character(0L))
    nms[vapply(seq_along(d), function(i) {
        z <- d[[i]]
        (is.factor(z) || is.character(z)) && !.BIOdry_is_month_col(z, nms[i])
    }, logical(1L))]
}

.BIOdry_time_cols <- function(d) {
    nms <- names(d)
    if (is.null(nms)) return(character(0L))
    nms[vapply(d, .BIOdry_is_whole, logical(1L))]
}

.BIOdry_response_cols <- function(d) {
    nms <- names(d)
    if (is.null(nms)) return(character(0L))
    nms[vapply(d, function(z) is.numeric(z) && !.BIOdry_is_whole(z), logical(1L))]
}

## TRUE when 'role' (e.g. "time", "groups", "response") was given explicitly
## in the defineDataRoles() call stored in the metadata
.BIOdry_declared <- function(meta, role) {
    is.list(meta) && is.call(meta$call) && !is.null(as.list(meta$call)[[role]])
}
