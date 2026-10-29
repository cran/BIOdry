plot.wlai <- structure(function # Plot Walter-Lieth climate diagrams
### A Walter-Lieth climate diagram of one annual block of a
### \code{\link{wlai}} object is drawn, with its aridity index.
                        ##details<<Precipitation (right axis, mm) and
                        ##temperature (left axis, degrees C) are drawn on
                        ##the Walter-Lieth scale computed by
                        ##\code{\link{wlai}}: 10 degrees C match 20 mm
                        ##(P = 2T), and precipitation above 100 mm is
                        ##compressed. Months whose precipitation curve lies
                        ##below the temperature curve form the dry season;
                        ##the others form the moist season, and
                        ##precipitation above 100 mm is filled in blue. The
                        ##aridity index (AI) is printed above the diagram,
                        ##followed by the grouping labels and the
                        ##\code{year} label of the block; that label
                        ##follows the labeling chosen in \code{\link{moveYr}} and
                        ##\code{\link{wlai}}: P44005.2001 for October 2000
                        ##to September 2001 with the default \code{'end'}.
                        ##
                        ##A \code{wlai} object computed on long-format data
                        ##(several plots or years) holds one annual block
                        ##per plot and period. Block names join the
                        ##grouping labels and the \code{hydro_year_id} of the block,
                        ##the calendar label of its first month: P44005.2000
                        ##holds October 2000 to September 2001. A block is
                        ##chosen by position or block name, by the labels
                        ##of the \code{year} or \code{hydro_year_id}
                        ##columns of the \code{wlai} table, or by a named
                        ##list of column values. Without a selection the
                        ##first block is drawn, with a warning; a selection
                        ##that matches several blocks stops with an error.
(
    x, ##<<A \code{\link{wlai}} object.
    y = NULL, ##<<Not used; kept for the \code{plot()} generic.
    which = NULL, ##<<\code{numeric} position or \code{character}
                  ##name of the block to draw.
    subset = NULL, ##<<Named \code{list} (or one-row
                   ##\code{data.frame}) of column values of the
                   ##\code{wlai} table selecting the block, such as the
                   ##plot label and a period label.
    year = NULL, ##<<Label of the \code{year} column of the block.
    hydro_year_id = NULL, ##<<Label of the \code{hydro_year_id} column
                          ##of the block.
    hatch = TRUE, ##<<\code{logical}. Hatch the dry and moist seasons;
                  ##if \code{FALSE}, fill them in gray.
    dry_density = 18, ##<<\code{numeric}. Hatching density of the dry
                      ##season, in lines per inch.
    moist_density = 18, ##<<\code{numeric}. Hatching density of the
                        ##moist season, in lines per inch.
    dry_angle = 45, ##<<\code{numeric}. Hatching angle of the dry
                    ##season, in degrees.
    moist_angle = -45, ##<<\code{numeric}. Hatching angle of the moist
                       ##season, in degrees.
    ... ##<<Further graphical parameters passed to
        ##\code{\link[graphics]{plot.default}}.
)
{
    if(!inherits(x, 'wlai'))
        stop("'x' does not belong to class 'wlai'")

    .resolve_block <- function(obj, which = NULL, subset = NULL, year = NULL, hydro_year_id = NULL){
        pl <- attributes(obj)
        have_geom <- all(c('xn','xn..','xn1','ptm','pr','pr.','tm','ai','cd') %in% names(pl))
        if(have_geom) return(pl)

        blocks <- attr(obj, 'blocks')
        if(is.null(blocks) || !length(blocks)) {
            stop("This 'wlai' object does not contain plottable Walter-Lieth geometry. Recompute with a patched wlai().")
        }

        if(!is.null(which)){
            if(is.character(which)){
                if(!(which %in% names(blocks))) stop("Requested block name not found in 'wlai' object.")
                return(blocks[[which]])
            }
            wi <- as.integer(which)[1L]
            if(is.na(wi) || wi < 1L || wi > length(blocks)) stop("'which' must select an existing climatic block.")
            return(blocks[[wi]])
        }

        map <- attr(obj, 'block_map')
        have_selector <- !is.null(year) || !is.null(hydro_year_id) || !is.null(subset)
        if(!is.null(map) && nrow(map) && have_selector){
            sel <- rep(TRUE, nrow(map))
            if(!is.null(year) && 'year' %in% names(map)) sel <- sel & map[['year']] %in% year
            if(!is.null(hydro_year_id) && 'hydro_year_id' %in% names(map)) sel <- sel & map[['hydro_year_id']] %in% hydro_year_id
            if(!is.null(subset)){
                if(is.data.frame(subset)) subset <- as.list(subset[1L, , drop = FALSE])
                if(!is.list(subset) || is.null(names(subset)) || !length(names(subset))) {
                    stop("'subset' must be a named list or one-row data.frame.")
                }
                for(nm in names(subset)){
                    if(!(nm %in% names(map))) stop(sprintf("Selector '%s' not found in wlai block metadata.", nm))
                    sel <- sel & map[[nm]] %in% subset[[nm]]
                }
            }
            hit <- which(sel)
            if(length(hit) == 1L) return(blocks[[hit]])
            if(length(hit) > 1L){
                lab <- map$.block_name[hit]
                stop(paste0("Selection matches multiple climatic blocks. Use 'which=' or a more specific selector. Matches: ", paste(lab, collapse = ', ')))
            }
            stop("No climatic block matched the requested selectors.")
        }

        if(length(blocks) > 1L) {
            warning("Multiple climatic blocks detected; plotting the first one. Use 'which=' or selectors such as year=/hydro_year_id=/subset= to choose another block.", call. = FALSE)
        }
        blocks[[1L]]
    }

    pl <- .resolve_block(x, which = which, subset = subset, year = year, hydro_year_id = hydro_year_id)
    fb <- function(xx){
        if(is.null(xx) || length(xx) == 0L) stop("Selected climatic block has no plottable monthly geometry.")
        nx <- c(pl[['ptm']], xx, pl[['ptm']])
        yy <- suppressWarnings(as.numeric(names(xx)))
        if(length(xx) == 0L || all(is.na(yy))) stop("Selected climatic block has no plottable monthly geometry.")
        names(nx) <- c(min(yy), names(xx), max(yy))
        nxn <- as.numeric(names(nx)) - 0.5
        data.frame(x = nxn, y = nx)
    }
    col_moist <- 'black'
    col_moist100 <- 'dodgerblue3'
    col_dry <- 'black'
    par(oma = c(0,0,0,2))
    ydata_max <- max(c(pl[['pr.']], pl[['tm']]), na.rm = TRUE)
    ydata_min <- pl[['ptm']]
    yrng <- ydata_max - ydata_min
    if(!is.finite(yrng) || yrng <= 0) yrng <- max(1, abs(ydata_max), abs(ydata_min), na.rm = TRUE)
    pin_h <- suppressWarnings(par('pin')[2])
    if(!is.finite(pin_h) || pin_h <= 0) pin_h <- 5
    cex_legend <- 0.8
    cex_text <- 0.8
    line_user <- par('csi') * (yrng / pin_h)
    if(!is.finite(line_user) || line_user <= 0) line_user <- 0.08 * yrng
    legend_pad <- 2.6 * cex_legend * line_user
    text_pad <- 1.2 * cex_text * line_user
    top_pad_user <- max(legend_pad + text_pad, 0.14 * yrng)
    ytop <- ydata_max + top_pad_user
    plot(fb(pl[['xn']]), ylim = c(ydata_min, ytop), col = col_moist, type = 'l',
         xaxt = 'n', yaxt = 'n', xlab = 'Month', ylab = NA, ...)
    mns <- pl[['month_labels']]
    if(is.null(mns) || !length(mns)) mns <- rownames(pl[['cd']])
    if(is.null(mns) || !length(mns)) mns <- month.abb[seq_len(NROW(pl[['cd']]))]
    xap <- unique(fb(pl[['pr']])[,1])
    yap <- pretty(c(ydata_min, ydata_max))
    axis(4, at = yap, labels = yap, las = 1)
    axis(2, at = yap, labels = yap/2, las = 1)
    axis(1, at = xap, tick = FALSE, labels = mns)
    axis(1, at = xap - 0.5, tick = TRUE, labels = FALSE)
    mtext(text = expression(~degree~C), las = 1, at = min(xap) - 1.2 * par('cex'))
    mtext(text = 'mm', las = 1, at = max(xap) + 1.2 * par('cex'))
    polygon(fb(pl[['xn']]), border = NA, col = col_moist100)
    if(isTRUE(hatch)) {
        polygon(fb(pl[['xn..']]), border = NA, density = moist_density, angle = moist_angle, col = col_moist)
        polygon(fb(pl[['tm']]), border = NA, density = dry_density, angle = dry_angle, col = col_dry)
        polygon(fb(pl[['xn1']]), border = 'black', col = 'white')
    } else {
        polygon(fb(pl[['xn..']]), border = NA, col = 'gray80')
        polygon(fb(pl[['tm']]), border = NA, col = 'gray35')
        polygon(fb(pl[['xn1']]), border = 'black', col = 'white')
    }
    pr_line <- fb(pl[['pr.']]); pr_line <- pr_line[2:(nrow(pr_line)-1L), , drop = FALSE]
    tm_line <- fb(pl[['tm']]);  tm_line <- tm_line[2:(nrow(tm_line)-1L), , drop = FALSE]
    lines(pr_line$x, pr_line$y, lwd = 1.2, col = 'black')
    lines(tm_line$x, tm_line$y, lwd = 1.2, col = 'black')
    lines(c(min(xap), max(xap)), c(100, 100), col = 'black', lwd = 1.2, lty = 1)
    legend_y <- ydata_max + top_pad_user - 0.55 * cex_legend * line_user
    text_y <- ydata_max + 0.55 * cex_text * line_user
    if(isTRUE(hatch)) {
        legend('top', legend = c('Dry season', 'Moist season', 'P > 100 mm'),
               fill = c(NA, NA, col_moist100), density = c(dry_density, moist_density, NA),
               angle = c(dry_angle, moist_angle, NA), border = c('black', 'black', NA),
               cex = cex_legend, horiz = TRUE, bty = 'n', inset = c(0, -0.01), xpd = NA)
    } else {
        legend('top', legend = c('Dry season', 'Moist season', 'P > 100 mm'),
               fill = c('gray35', 'gray80', col_moist100), cex = cex_legend, horiz = TRUE, border = NA, bty = 'n', inset = c(0, -0.01), xpd = NA)
    }
    rai <- round(pl[['ai']], 3)
    lab <- pl[['block_name']]
    ## the printed label joins the grouping labels and the year column of
    ## the selected block, so that it follows label_year of moveYr()/wlai()
    ## (block names keep hydro_year_id)
    map <- attr(x, 'block_map')
    if(!is.null(lab) && !is.null(map) && 'year' %in% names(map)) {
        k <- match(lab, map$.block_name)
        if(!is.na(k)) {
            grp <- setdiff(names(map), c('AI', 'hydro_year_id', 'year', '.block_name'))
            lab <- paste(c(vapply(map[k, grp, drop = FALSE], as.character, ''),
                           as.character(map$year[k])), collapse = '.')
        }
    }
    txt <- paste('AI = ', rai, sep = '')
    if(!is.null(lab) && nzchar(lab)) txt <- paste0(txt, '  [', lab, ']')
    text(x = 1.5, y = text_y, cex = cex_text, col = col_dry, labels = txt, adj = c(0, 0.5), xpd = NA)
### Called for its side effect, the diagram; \code{NULL} is returned
### invisibly.
},
ex=function() {
    ## monthly precipitation and temperature of plot P44005 (PTclim05);
    ## hydrological years from October to September, labeled by the
    ## calendar year in which they end
    data(PTclim05, envir = environment())
    ai <- subset(PTclim05, plot == 'P44005') |>
        moveYr() |>
        wlai()
    subset(ai, year == 2001)
    ## the diagram of October 2000 to September 2001
    plot(ai, year = 2001)
    ## the same block, with gray fills
    plot(ai, hydro_year_id = 2000, hatch = FALSE)
})
