moveYr <- structure(function#Seasonal years
### Monthly records in time-series replicates (usually of climate) are
### labeled for the years can begin in a month other than January.
                    ##details<<\code{character} months as defined in
                    ##\code{\link{month.abb}} or \code{\link{month.name}}.
                    ##The columns are found by the package's column
                    ##names: \code{year}, and \code{month} (month
                    ##abbreviations, full month names, or integers
                    ##1:12); a factor whose levels are month names is
                    ##also accepted as the month column.
(
    cd, ##<<\code{data.frame} of monthly records with the columns
        ##\code{year} and \code{month}, and any grouping or value
        ##columns; or a \code{numeric} vector of years named by month.
    ini.mnt = 'Oct', ##<<\code{character} or \code{numeric}. First
                     ##month of the hydrological year, as a month
                     ##abbreviation, full month name or number from 1 to
                     ##12.
    label_year = c('end', 'start'), ##<<\code{character}. How
                                     ##hydrological years are labeled
                                     ##in the column \code{year}.
                                     ##\code{'end'} (default) labels each
                                     ##year by
                                     ##the calendar year in which it
                                     ##ends; \code{'start'} labels it by
                                     ##the calendar year in which it
                                     ##starts. The column
                                     ##\code{hydro_year_id} always holds
                                     ##the start year.
    keep_hydro_id = TRUE, ##<<\code{logical}. Add a column with the
                          ##calendar year in which each hydrological
                          ##year starts (data frames only).
    hydro_id_name = 'hydro_year_id', ##<<\code{character}. Name of that
                                     ##column. Other functions of the
                                     ##package, such as \code{\link{wlai}},
                                     ##find it only under the default
                                     ##name.
    validate = c('none','warn','stop') ##<<\code{character}. What to do
                                       ##when month labels cannot be
                                       ##read, years are not whole
                                       ##numbers, several month columns
                                       ##are found, or the rows are not
                                       ##sorted by group, year and month:
                                       ##\code{'none'} (default) goes on
                                       ##silently (unreadable months
                                       ##become \code{NA}), \code{'warn'}
                                       ##warns and \code{'stop'} stops.
) {

    validate <- match.arg(validate)
    label_year <- match.arg(label_year)
    signal <- function(msg){
        if(validate == 'warn') warning(msg, call. = FALSE)
        if(validate == 'stop') stop(msg, call. = FALSE)
        invisible(NULL)
    }

    chn <- function(tmp){
        if(length(tmp) == 0L) return(setNames(integer(0), character(0)))
        raw <- tmp
        if(is.factor(raw)) raw <- as.character(raw)

        if(is.numeric(raw)){
            out <- suppressWarnings(as.integer(raw))
            bad <- !is.na(raw) & (is.na(out) | !(out %in% 1:12))
            if(any(bad)) signal("Numeric month values must be integers from 1 to 12.")
            out[bad] <- NA_integer_
            names(out) <- month.abb[out]
            return(out)
        }

        chr <- trimws(as.character(raw))
        out <- rep.int(NA_integer_, length(chr))
        good <- !is.na(chr) & nzchar(chr)
        low <- tolower(chr[good])
        abb_map <- setNames(1:12, tolower(month.abb))
        name_map <- setNames(1:12, tolower(month.name))
        punct_low <- gsub('[[:punct:]]+', '', low)
        num_try <- suppressWarnings(as.integer(punct_low))

        idx <- rep.int(NA_integer_, sum(good))
        is_num <- !is.na(num_try) & num_try %in% 1:12
        idx[is_num] <- num_try[is_num]

        need <- is.na(idx)
        if(any(need)){
            low_need <- low[need]
            punct_need <- punct_low[need]
            idx[need] <- abb_map[low_need]
            still <- is.na(idx)
            if(any(still)) idx[still] <- abb_map[punct_need[still]]
            still <- is.na(idx)
            if(any(still)) idx[still] <- name_map[low_need[still]]
            still <- is.na(idx)
            if(any(still)) idx[still] <- name_map[punct_need[still]]
        }

        out[good] <- idx
        bad <- good & is.na(out)
        if(any(bad)) signal("Month values could not be interpreted. Use month abbreviations, full month names, or integers 1:12.")
        names(out) <- month.abb[out]
        out
    }

    ## the package's column contract: the year column is 'year'
    choose_year_col <- function(df){
        intersect('year', names(df))
    }

    is_month_col <- function(x){
        if(is.factor(x)){
            lev <- levels(x)
            if(length(lev) == 0L) return(FALSE)
            idx <- suppressWarnings(chn(lev))
            return(all(is.na(lev) | !nzchar(lev) | !is.na(idx)))
        }
        if(is.character(x)){
            chr <- x[!is.na(x)]
            if(length(chr) == 0L) return(FALSE)
            idx <- suppressWarnings(chn(chr))
            return(all(!is.na(idx)))
        }
        if(is.numeric(x)){
            nn <- x[!is.na(x)]
            if(length(nn) == 0L) return(FALSE)
            out <- suppressWarnings(as.integer(nn))
            return(all(!is.na(out) & out %in% 1:12))
        }
        FALSE
    }

    ## the month column is 'month', or a factor whose levels are month
    ## names (see cClass)
    choose_month_col <- function(df){
        nms <- names(df)
        if('month' %in% nms && is_month_col(df[['month']])) return('month')
        cand <- nms[vapply(seq_along(df), function(i)
            is.factor(df[[i]]) && .BIOdry_is_month_col(df[[i]], nms[i]), logical(1L))]
        if(length(cand) > 1L)
            signal(paste("Multiple month columns detected:", paste(cand, collapse = ', '), "- using", cand[1L]))
        cand[1L][!is.na(cand[1L])]
    }

    make_hydro_id <- function(year_vals, month_idx, ini_idx){
        year_vals <- suppressWarnings(as.integer(year_vals))
        month_idx <- suppressWarnings(as.integer(month_idx))
        out <- year_vals
        sel <- !is.na(year_vals) & !is.na(month_idx)
        out[sel & month_idx < ini_idx] <- year_vals[sel & month_idx < ini_idx] - 1L
        out
    }

    label_from_hydro_id <- function(hydro_id, label_year){
        out <- hydro_id
        if(label_year == 'end') out <- hydro_id + 1L
        out
    }

    isdf <- is.data.frame(cd)
    ini_idx <- chn(ini.mnt)
    ini_idx <- unname(ini_idx[1L])
    if(length(ini_idx) != 1L || is.na(ini_idx) || !(ini_idx %in% 1:12)) stop("'ini.mnt' must identify a valid month.", call. = FALSE)
    if(!is.logical(keep_hydro_id) || length(keep_hydro_id) != 1L || is.na(keep_hydro_id)) stop("'keep_hydro_id' must be TRUE or FALSE.", call. = FALSE)
    if(!is.character(hydro_id_name) || length(hydro_id_name) != 1L || is.na(hydro_id_name) || !nzchar(hydro_id_name)) stop("'hydro_id_name' must be a non-empty character scalar.", call. = FALSE)

    if(isdf){
        ny <- as.data.frame(cd, stringsAsFactors = FALSE)
        year_col <- choose_year_col(ny)
        if(length(year_col) == 0L) stop("moveYr() requires a column named 'year'.", call. = FALSE)
        month_col <- choose_month_col(ny)
        if(length(month_col) == 0L) stop("moveYr() requires a column named 'month' (month abbreviations, full month names, or integers 1:12), or a factor whose levels are month names.", call. = FALSE)

        yr_raw <- ny[[year_col]]
        yr_num <- suppressWarnings(as.integer(as.character(yr_raw)))
        bad_year <- !is.na(yr_raw) & is.na(yr_num)
        if(any(bad_year)) signal(paste("Year column", sQuote(year_col), "contains non-integer values."))

        m_idx <- chn(ny[[month_col]])
        month_num <- unname(m_idx)
        if(all(is.na(month_num))) stop("Detected month column could not be interpreted.", call. = FALSE)

        if(validate != 'none'){
            grp_cols <- setdiff(names(ny)[vapply(ny, function(x) is.factor(x) || is.character(x), logical(1L))], month_col)
            ord <- do.call(order, c(unname(ny[grp_cols]), list(yr_num, month_num, na.last = TRUE)))
            if(length(ord) == nrow(ny) && !identical(ord, seq_len(nrow(ny)))) signal("Input appears unsorted relative to grouping columns, year, and month. Verify group-wise ordering before using moveYr().")
        }

        hydro_id <- make_hydro_id(yr_num, month_num, ini_idx)
        year_lab <- label_from_hydro_id(hydro_id, label_year)

        if(year_col != 'year' && !('year' %in% names(ny))){
            names(ny)[names(ny) == year_col] <- 'year'
            year_col <- 'year'
        }
        ny[[year_col]] <- year_lab

        month_factor <- factor(month.abb[month_num], levels = month.abb)
        if(month_col != 'month' && !('month' %in% names(ny))){
            names(ny)[names(ny) == month_col] <- 'month'
            month_col <- 'month'
        }
        ny[[month_col]] <- month_factor

        if(isTRUE(keep_hydro_id)) ny[[hydro_id_name]] <- hydro_id
        attr(ny, 'hydro_year_id') <- hydro_id
        attr(ny, 'hydro_year_id_name') <- if(isTRUE(keep_hydro_id)) hydro_id_name else NULL
        attr(ny, 'hydro_year_label') <- label_year
        attr(ny, 'hydro_year_start_month') <- month.abb[ini_idx]
        return(ny)
    }

    m_idx <- chn(names(cd))
    month_num <- unname(m_idx)
    if(all(is.na(month_num))) stop("Named vectors passed to moveYr() must use month names or month numbers as names.", call. = FALSE)
    yr_num <- suppressWarnings(as.integer(cd))
    bad_year <- !is.na(cd) & is.na(yr_num)
    if(any(bad_year)) signal("Vector values should be integer-like years.")

    hydro_id <- make_hydro_id(yr_num, month_num, ini_idx)
    ny <- label_from_hydro_id(hydro_id, label_year)
    names(ny) <- month.abb[month_num]
    attr(ny, 'hydro_year_id') <- hydro_id
    attr(ny, 'hydro_year_label') <- label_year
    attr(ny, 'hydro_year_start_month') <- month.abb[ini_idx]
    ny
### For a data frame, the input with the month column as a factor of
### month abbreviations, the column \code{year} relabeled by
### hydrological year (see label_year), and, by default, a column
### \code{hydro_year_id} with the start year of each hydrological year.
### The calendar year is not kept: with ini.mnt = 'Oct' and
### label_year = 'end', October to December 1955 are labeled 1956. It
### can be recovered as \code{hydro_year_id} for the months from
### ini.mnt to December and \code{hydro_year_id + 1} for the months
### before ini.mnt.
}, ex=function() {
    ## monthly precipitation and temperature of two plots (PTclim05)
    data(PTclim05, envir = environment())
    ## hydrological years from October to September, labeled by the
    ## calendar year in which they end (label_year = 'end', the default)
    cl <- PTclim05 |> moveYr()
    head(cl)
    ## October to December 1955 belong to the hydrological year 1956
    subset(cl, plot == 'P44005' & hydro_year_id == 1955 &
               month %in% c('Oct', 'Nov', 'Dec', 'Jan'))
    ## the calendar year is recovered from hydro_year_id (see Value)
    cl$calendar_year <- cl$hydro_year_id +
        (match(as.character(cl$month), month.abb) < 10)
    all(cl$calendar_year[order(cl$plot, cl$calendar_year, cl$month)] ==
        PTclim05$year[order(PTclim05$plot, PTclim05$year, PTclim05$month)])
    ## labels by the start year, and hydrological years from April
    PTclim05 |> moveYr(label_year = 'start') |> head(3)
    PTclim05 |> moveYr(ini.mnt = 'Apr') |> head(3)
})
