wlai <- structure(function #Walter-Lieth aridity index
### Computing the annual aridity index from Walter-Lieth climate diagrams
(
    cd, ##<<\code{data.frame}. Multilevel climatic data series of
        ##monthly precipitation sums (mm), and monthly average
        ##temperatures (degree C). Precipitation is the column
        ##\code{pr} and temperature the column \code{tm}; inputs declared
        ##with \code{\link{defineDataRoles}} take precedence: declared
        ##\code{pr} and \code{tm} are used by name, other declared
        ##inputs in their order, precipitation first. If \code{pr} or
        ##\code{tm} is missing, the first two numeric columns are used
        ##and a warning names them. The month column is found by the
        ##package's column name \code{month} (a factor whose levels are
        ##month names is also accepted), and the year column by
        ##\code{year}, or \code{hydro_year_id} as returned by
        ##\code{\link{moveYr}}; without them a single whole-number
        ##column is used as the year. If none of \code{year},
        ##\code{hydro_year_id}, \code{month} or \code{time} is present
        ##(and the row names are not month names), a warning is given.
    sqt = TRUE, ##<<\code{logical}. Print the square root of the
                ##aridity index.
    ini.mnt = NULL, ##<<\code{character} or \code{numeric}. Optional
                    ##start month to define annual blocks in long
                    ##climatic data. When supplied, yearly labels are
                    ##shifted before computing aridity indexes. Without
                    ##\code{ini.mnt}, calendar blocks start at the first
                    ##month of the earliest year; if the data have no
                    ##year column, the month of the first row is used,
                    ##so rows should then be in time order.
    label_year = c('end', 'start'), ##<<\code{character}. How
                                     ##hydrological years are labeled
                                     ##in the output column
                                     ##\code{year}. \code{'end'}
                                     ##(default) labels each year by the
                                     ##calendar
                                     ##year in which it ends, so October
                                     ##2000 to September 2001 is labeled
                                     ##2001, the year of ring formation;
                                     ##\code{'start'} labels it by the
                                     ##calendar year in which it starts.
                                     ##It applies when
                                     ##\code{ini.mnt} is supplied or when
                                     ##\code{cd} already has a
                                     ##\code{hydro_year_id} column, as
                                     ##returned by \code{moveYr}. The
                                     ##output column \code{hydro_year_id}
                                     ##always holds the start year. If
                                     ##the \code{year} column of \code{cd}
                                     ##follows the other convention, a
                                     ##warning is given and
                                     ##\code{label_year} decides. Without
                                     ##hydrological years the years are
                                     ##calendar years and
                                     ##\code{label_year} has no effect.
    validate = c('none','warn','stop') ##<<\code{character}.
                                        ##Validation mode.
)
{
    label_year <- match.arg(label_year)
    validate <- match.arg(validate)

    .signal <- function(msg){
        if(identical(validate, 'warn'))
            warning(msg, call. = FALSE)
        if(identical(validate, 'stop'))
            stop(msg, call. = FALSE)
        invisible(NULL)
    }

    .month_num <- function(x){
        if(is.factor(x)) x <- as.character(x)
        if(is.numeric(x) || is.integer(x)){
            xi <- as.integer(x)
            ok <- !is.na(xi) & xi >= 1L & xi <= 12L
            if(!all(ok)) return(rep(NA_integer_, length(xi)))
            return(xi)
        }
        if(is.character(x)){
            xs <- trimws(tolower(x))
            abb <- tolower(month.abb)
            full <- tolower(month.name)
            out <- rep(NA_integer_, length(xs))
            hit_num <- suppressWarnings(as.integer(xs))
            ok_num <- !is.na(hit_num) & hit_num >= 1L & hit_num <= 12L
            out[ok_num] <- hit_num[ok_num]
            rem <- is.na(out)
            if(any(rem)){
                key <- substr(xs[rem], 1L, 3L)
                m1 <- match(key, abb)
                m2 <- match(xs[rem], full)
                out[rem] <- ifelse(!is.na(m1), m1, m2)
            }
            return(as.integer(out))
        }
        rep(NA_integer_, length(x))
    }

    ## the month column is 'month', or a factor whose levels are month
    ## names (see cClass)
    .detect_month_col <- function(df){
        nms <- names(df)
        if('month' %in% nms) return('month')
        ok <- vapply(seq_along(df), function(i)
            is.factor(df[[i]]) && .BIOdry_is_month_col(df[[i]], nms[i]), logical(1))
        if(sum(ok) > 1L) .signal("Multiple month columns detected; using the first one.")
        if(any(ok)) nms[ok][1L] else NULL
    }

    ## precipitation is 'pr' and temperature is 'tm' (the package's column
    ## contract); inputs declared with defineDataRoles() take precedence
    ## (pr and tm by name if declared, otherwise precipitation first, then
    ## temperature); otherwise the first two
    ## numeric columns are used, with a warning
    .detect_clim_cols <- function(df){
        nms <- names(df)
        meta <- attr(df, 'BIOdry_meta', exact = TRUE)
        if(.BIOdry_declared(meta, 'inputs')){
            decl <- intersect(as.character(meta$inputs), nms)
            if(all(c('pr', 'tm') %in% decl)) return(c(pr = 'pr', tm = 'tm'))
            if(length(decl) >= 2L) return(c(pr = decl[1L], tm = decl[2L]))
        }
        if(all(c('pr', 'tm') %in% nms)) return(c(pr = 'pr', tm = 'tm'))
        num_cols <- nms[vapply(df, is.numeric, logical(1))]
        if(length(num_cols) < 2L) return(c(pr = NA_character_, tm = NA_character_))
        warning(sprintf(paste0("wlai(): no columns named 'pr' and 'tm'; using ",
                               "'%s' as precipitation and '%s' as temperature ",
                               "(the first two numeric columns)."),
                        num_cols[1L], num_cols[2L]), call. = FALSE)
        c(pr = num_cols[1L], tm = num_cols[2L])
    }

    .detect_key_col <- function(df, month_col, pr_col, tm_col){
        nms <- names(df)
        low <- tolower(nms)
        ## the package's column contract: 'hydro_year_id' (from moveYr) or
        ## 'year'; otherwise a single whole-number column
        pref <- c('hydro_year_id','year')
        hit <- nms[match(pref, low, nomatch = 0L)]
        hit <- hit[nzchar(hit)]
        if(length(hit)) return(hit[1L])
        rem <- setdiff(nms, c(month_col, pr_col, tm_col))
        intish <- rem[vapply(df[rem], function(z){
            if(!(is.numeric(z) || is.integer(z))) return(FALSE)
            y <- z[!is.na(z)]
            length(y) > 0L && all(abs(y - round(y)) < .Machine$double.eps^0.5)
        }, logical(1))]
        if(length(intish) == 1L) return(intish[1L])
        NULL
    }

    .detect_group_cols <- function(df, month_col, key_col, pr_col, tm_col){
        ## factor and character columns that are not months (see cClass)
        setdiff(.BIOdry_group_cols(df),
                c(month_col, key_col, pr_col, tm_col, 'hydro_year_id'))
    }

    .shift_years <- function(df, month_col, key_col, ini.mnt, label_year){
        m0 <- .month_num(ini.mnt)
        if(length(m0) != 1L || is.na(m0)){
            .signal("'ini.mnt' is not a valid month; yearly labels were not shifted.")
            return(df)
        }
        m <- .month_num(df[[month_col]])
        if(any(is.na(m))){
            .signal("Month values could not be interpreted; yearly labels were not shifted.")
            return(df)
        }
        yr <- suppressWarnings(as.integer(df[[key_col]]))
        if(any(is.na(yr))){
            .signal("Year values could not be interpreted; yearly labels were not shifted.")
            return(df)
        }
        hydro_id <- ifelse(m < m0, yr - 1L, yr)
        if(identical(label_year, 'start')){
            df[[key_col]] <- hydro_id
        } else {
            df[[key_col]] <- hydro_id + 1L
        }
        df$hydro_year_id <- hydro_id
        df
    }

    .geom_ai <- function(pr, tm, sqt){
        tm2 <- 2 * tm
        names(pr) <- seq_along(pr)
        names(tm2) <- names(pr)

        fint <- function(pr, tm){
            ix <- which(diff(pr > tm) != 0)
            if(!length(ix)) return(numeric(0))
            pr.m <- pr[ix + 1L] - pr[ix]
            tm.m <- tm[ix + 1L] - tm[ix]
            fx <- (tm[ix] - pr[ix]) / (pr.m - tm.m)
            ixc <- ix + fx
            prc <- pr[ix] + (pr.m * (ixc - ix))
            names(prc) <- ixc
            prc
        }

        ford <- function(x) order(as.numeric(names(x)))

        pr_scaled <- ifelse(pr > 100, 80 + 0.2 * pr, pr)
        pr_capped <- ifelse(pr > 100, 100, pr)
        pics <- fint(rep(100, length(pr)), pr)
        prc <- fint(pr, tm2)

        fxn <- function(x3, x4){
            xn <- c(ifelse(pr > tm2, x3, x4), prc)
            xn[ford(xn)]
        }

        xn <- fxn(pr_scaled, tm2)
        xn <- c(xn, pics)[ford(c(xn, pics))]
        xn1 <- fxn(tm2, pr_scaled)
        xn_capped <- fxn(pr_capped, tm2)
        xn_capped <- c(xn_capped, pics)[ford(c(xn_capped, pics))]
        pr_plot <- c(pr_scaled, pics)[ford(c(pr_scaled, pics))]

        fb <- function(x){
            ptm <- min(pr_plot, tm2)
            nx <- c(ptm, x, ptm)
            y <- as.numeric(names(x))
            names(nx) <- c(min(y), names(x), max(y))
            nxn <- as.numeric(names(nx)) - 0.5
            data.frame(x = nxn, y = nx)
        }

        surf <- function(y){
            x <- y[,1]; yy <- y[,2]
            dosarea <- sapply(2:(length(yy)-1L), function(i) yy[i] * (x[i+1L] - x[i-1L]))
            0.5 * sum(dosarea)
        }

        ptm <- min(pr_plot, tm2)
        wet <- surf(fb(xn)) - surf(fb(tm2))
        dry <- surf(fb(xn)) - surf(fb(pr_plot))
        AI <- dry / wet
        if(isTRUE(sqt)) AI <- sqrt(AI)

        list(ai = AI, xn = xn, xn.. = xn_capped, xn1 = xn1,
             ptm = ptm, pr = pr, pr. = pr_plot, tm = tm2)
    }

    .block_out <- function(df, pr_col, tm_col, month_col, key_col, group_cols,
                           sqt, attach_attrs = TRUE, start_month = 1L,
                           year_offset = NULL){
        pr <- df[[pr_col]]
        tm <- df[[tm_col]]
        month_num <- if(!is.null(month_col)) .month_num(df[[month_col]]) else NULL

        if(!is.null(month_num)){
            good_months <- all(!is.na(month_num))
            if(isTRUE(good_months) && length(month_num) == 12L && length(unique(month_num)) == 12L){
                start_month <- suppressWarnings(as.integer(start_month)[1L])
                if(is.na(start_month) || start_month < 1L || start_month > 12L) start_month <- 1L
                ord <- order(((month_num - start_month) %% 12L), month_num)
                df <- df[ord, , drop = FALSE]
                pr <- pr[ord]
                tm <- tm[ord]
                month_num <- month_num[ord]
            }
            if(!good_months){
                .signal("Some month values could not be interpreted in a climatic block.")
                AI <- NA_real_
                geom <- NULL
            } else if(length(month_num) != 12L || length(unique(month_num)) != 12L){
                .signal("A climatic block should contain exactly 12 unique months; returning NA for this block.")
                AI <- NA_real_
                geom <- NULL
            } else {
                geom <- .geom_ai(pr = pr, tm = tm, sqt = sqt)
                AI <- geom$ai
            }
        } else {
            if(length(pr) < 12L){
                .signal("At least 12 monthly observations are required to compute wlai().")
                AI <- NA_real_
                geom <- NULL
            } else {
                geom <- .geom_ai(pr = pr, tm = tm, sqt = sqt)
                AI <- geom$ai
            }
        }

        out_cols <- unique(c(key_col, group_cols))
        out_cols <- out_cols[nzchar(out_cols)]
        meta <- if(length(out_cols)) {
            keep <- df[1L, out_cols, drop = FALSE]
            rownames(keep) <- NULL
            keep
        } else data.frame(.row = 1L)[, FALSE, drop = FALSE]
        res <- cbind(data.frame(AI = AI), meta)
        if(!is.null(year_offset) && 'hydro_year_id' %in% names(res)){
            year_lab <- as.integer(res$hydro_year_id) + year_offset
            rest <- setdiff(names(res), c('AI', 'hydro_year_id', 'year'))
            res <- cbind(res[, c('AI', 'hydro_year_id'), drop = FALSE],
                         year = year_lab, res[, rest, drop = FALSE])
        }

        if(isTRUE(attach_attrs) && !is.null(geom)){
            cd_plot <- data.frame(pr = pr, tm = tm)
            month_labels <- NULL
            if(!is.null(month_col)) {
                month_labels <- month.abb[month_num]
                rownames(cd_plot) <- month_labels
            }
            attributes(res) <- c(attributes(res),
                                 list(xn = geom$xn,
                                      xn.. = geom$xn..,
                                      xn1 = geom$xn1,
                                      ptm = geom$ptm,
                                      pr = geom$pr,
                                      pr. = geom$pr.,
                                      r.n = rownames(df),
                                      tm = geom$tm,
                                      ai = geom$ai,
                                      cd = cd_plot,
                                      month_labels = month_labels,
                                      hydro_year_start_month = start_month))
            class(res) <- c('wlai', class(res))
        }
        res
    }

    if(!is.data.frame(cd)){
        cd <- as.data.frame(cd, stringsAsFactors = FALSE)
    }
    if(!nrow(cd) || ncol(cd) < 2L){
        .signal("'cd' must contain at least two climate variables.")
        return(structure(data.frame(AI = NA_real_), class = 'data.frame'))
    }

    clim <- .detect_clim_cols(cd)
    pr_col <- clim[['pr']]
    tm_col <- clim[['tm']]
    if(is.na(pr_col) || is.na(tm_col) || !nzchar(pr_col) || !nzchar(tm_col)){
        .signal("Could not detect precipitation and temperature columns.")
        return(structure(data.frame(AI = NA_real_), class = 'data.frame'))
    }

    month_col <- .detect_month_col(cd)
    if(is.null(month_col) && nrow(cd) == 12L){
        rn_month <- .month_num(rownames(cd))
        if(length(rn_month) == 12L && all(!is.na(rn_month)) && length(unique(rn_month)) == 12L){
            cd$.month_from_rownames <- rn_month
            month_col <- '.month_from_rownames'
        }
    }
    key_col <- .detect_key_col(cd, month_col, pr_col, tm_col)
    if(is.null(month_col) &&
       !any(c('year', 'hydro_year_id', 'month', 'time') %in% names(cd))){
        warning(paste0("wlai(): none of the columns 'year', 'hydro_year_id', ",
                       "'month' or 'time' was found (nor month names in a ",
                       "factor or in the row names); the rows are taken as ",
                       "they are."), call. = FALSE)
    }

    start_month_global <- suppressWarnings(.month_num(ini.mnt)[1L])
    if(is.na(start_month_global)) {
        start_month_global <- suppressWarnings(.month_num(attr(cd, 'hydro_year_start_month'))[1L])
    }
    if(is.na(start_month_global)) {
        start_month_global <- suppressWarnings(.month_num(attr(cd, 'BIOdry_meta')$hydro_year_start_month)[1L])
    }
    if(is.na(start_month_global) && !is.null(month_col)) {
        m_first <- .month_num(cd[[month_col]])
        ## with a year column, take the first month of the earliest year, so
        ## that the result does not depend on the row order of 'cd'
        if(!is.null(key_col)) {
            yr <- suppressWarnings(as.numeric(as.character(cd[[key_col]])))
            m_first <- m_first[order(yr, m_first)]
        }
        m_first <- m_first[!is.na(m_first)]
        if(length(m_first)) start_month_global <- m_first[1L]
    }
    if(is.na(start_month_global)) start_month_global <- 1L

    if(is.null(ini.mnt) && all(c('hydro_year_id', 'year') %in% names(cd))){
        in_offset <- unique(suppressWarnings(
            as.integer(cd$year) - as.integer(cd$hydro_year_id)))
        in_label <- if(identical(in_offset, 0L)) 'start'
                    else if(identical(in_offset, 1L)) 'end' else NA_character_
        if(!is.na(in_label) && !identical(in_label, label_year)){
            warning(sprintf(paste0("The 'year' column of 'cd' labels hydrological ",
                                   "years by their %s year (moveYr(label_year = '%s')), ",
                                   "but wlai() uses label_year = '%s'; wlai's ",
                                   "label_year decides the output 'year' column."),
                            in_label, in_label, label_year), call. = FALSE)
        }
    }
    if(!is.null(ini.mnt) && !is.null(month_col) && !is.null(key_col)){
        cd <- .shift_years(cd, month_col, key_col, ini.mnt, label_year)
        if('hydro_year_id' %in% names(cd)) key_col <- 'hydro_year_id'
    }
    year_offset <- NULL
    if(identical(key_col, 'hydro_year_id')){
        year_offset <- if(identical(label_year, 'end')) 1L else 0L
    }
    if('hydro_year_start_month' %in% names(attributes(cd))){
        start_month_global <- suppressWarnings(.month_num(attr(cd, 'hydro_year_start_month'))[1L])
        if(is.na(start_month_global)) start_month_global <- 1L
    }

    group_cols <- .detect_group_cols(cd, month_col, key_col, pr_col, tm_col)

    needs_split <- FALSE
    if(!is.null(month_col) && nrow(cd) > 12L){
        m <- .month_num(cd[[month_col]])
        needs_split <- any(duplicated(m)) || !is.null(key_col) || length(group_cols) > 0L
    }

    if(!needs_split){
        return(.block_out(cd, pr_col, tm_col, month_col, key_col, group_cols,
                          sqt = sqt, attach_attrs = TRUE, start_month = start_month_global,
                          year_offset = year_offset))
    }

    split_cols <- unique(c(group_cols, key_col))
    split_cols <- split_cols[!is.na(split_cols) & nzchar(split_cols)]
    if(!length(split_cols)){
        .signal("Long climatic input was detected but no split key was available; computing one overall AI.")
        return(.block_out(cd, pr_col, tm_col, month_col, key_col, group_cols,
                          sqt = sqt, attach_attrs = TRUE, start_month = start_month_global,
                          year_offset = year_offset))
    }

    fac <- interaction(cd[split_cols], drop = TRUE, lex.order = TRUE)
    idx <- split(seq_len(nrow(cd)), fac, drop = TRUE)
    block_names <- names(idx)
    res_list <- lapply(seq_along(idx), function(j){
        i <- idx[[j]]
        blk <- .block_out(cd[i, , drop = FALSE], pr_col, tm_col, month_col,
                          key_col = key_col, group_cols = group_cols,
                          sqt = sqt, attach_attrs = TRUE, start_month = start_month_global,
                          year_offset = year_offset)
        attr(blk, 'block_name') <- block_names[j]
        blk
    })
    row_has_geom <- vapply(res_list, function(z) !is.null(attr(z, 'xn')), logical(1))
    blocks <- lapply(res_list[row_has_geom], function(z){
        at <- attributes(z)
        at[c('xn','xn..','xn1','ptm','pr','pr.','r.n','tm','ai','cd','block_name')]
    })
    if(length(blocks)) names(blocks) <- vapply(blocks, function(z) {
        nm <- z[['block_name']]
        if(is.null(nm) || !nzchar(nm)) '' else as.character(nm)
    }, character(1))
    out <- do.call(rbind, lapply(res_list, function(z){
        at <- attributes(z)
        drop_at <- intersect(names(at), c('xn','xn..','xn1','ptm','pr','pr.','r.n','tm','ai','cd','block_name'))
        for(nm in drop_at) attr(z, nm) <- NULL
        class(z) <- setdiff(class(z), 'wlai')
        z
    }))
    rownames(out) <- NULL
    if(length(blocks)) attr(out, 'blocks') <- blocks
    if(any(row_has_geom)) {
        block_map <- do.call(rbind, lapply(res_list[row_has_geom], function(z){
            zz <- z
            for(nm in intersect(names(attributes(zz)), c('xn','xn..','xn1','ptm','pr','pr.','r.n','tm','ai','cd','block_name','month_labels','hydro_year_start_month'))) attr(zz, nm) <- NULL
            class(zz) <- setdiff(class(zz), 'wlai')
            zz$.block_name <- attr(z, 'block_name')
            zz
        }))
        rownames(block_map) <- NULL
        attr(out, 'block_map') <- block_map
    }
    class(out) <- c('wlai', setdiff(class(out), 'wlai'))
    out
### A \code{data.frame} of class \code{wlai} with one row per annual
### block: the aridity index \code{AI}, the year columns
### (\code{hydro_year_id} and \code{year} when the input comes from
### \code{\link{moveYr}}) and the grouping columns. The Walter-Lieth
### geometry of each block is kept in attributes and drawn by
### \code{\link{plot.wlai}}; no plot is drawn here.
},
ex=function() {
    ## monthly precipitation (pr, mm) and temperature (tm, degrees C) of
    ## two plots, 1955-2005 (PTclim05)
    data(PTclim05, envir = environment())
    ## aridity index of one calendar year of one plot
    subset(PTclim05, plot == 'P44005' & year == 2000) |> wlai()
    ## one index per plot and hydrological year (October to September),
    ## labeled by the year in which it ends; the first and last
    ## hydrological years are incomplete and get NA
    ai <- PTclim05 |>
        moveYr() |>
        wlai()
    head(ai)
    subset(ai, is.na(AI))
    ## the Walter-Lieth diagram of one block (see plot.wlai)
    plot(ai, subset = list(plot = 'P44005', year = 2001))
})
