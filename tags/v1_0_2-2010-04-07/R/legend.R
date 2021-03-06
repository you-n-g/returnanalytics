`legend` <-
function (x, y = NULL, legend, fill = NULL, col = par("col"),
    lty, lwd, pch, angle = 45, density = NULL, bty = "o", bg = par("bg"),
    pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, xjust = 0,
    yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
    text.width = NULL, text.col = par("col"), merge = do.lines &&
        has.pch, trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE,
    title = NULL, inset = 0, border.col = NULL, border.lwd = 1, border.lty = "solid", box.col = NULL, box.lwd = 1, box.lty = "solid")
{
    # Modifications to core graphics legend() function
    # @author R Core Dev Team
    # @author modifications Peter Carl

    # Minor modifications to the function include:
    # - added border.col so that the legend border could be colored
    # - added border.lwd to change the line width of the border
    # - added border.lty to change the line type for the border
    # - changed line segment end to a more squared type

    # > plot.new()
    # > par(mar = c(0, 0, 0, 0))
    # > legend("center",text.col=rainbow6equal, cex = .8, ncol=3, border.col = "grey",legend = colnames(data))

    if (missing(legend) && !missing(y) && (is.character(y) ||
        is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    if (length(title) > 1)
        stop("invalid title")
    n.leg <- if (is.call(legend))
        1
    else length(legend)
    if (n.leg == 0)
        stop("'legend' is of length 0")
    auto <- if (is.character(x))
        match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2)
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle, border = border.col, lty = border.lty, lwd = border.lwd, ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density, border = border, lty = lty, lwd = lwd, ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, lend="butt", ...) # added squared end to line seg
    }
    points2 <- function(x, y, ...) {
        if (xlog)
            x <- 10^x
        if (ylog)
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog)
            x <- 10^x
        if (ylog)
            y <- 10^y
        text(x, y, ...)
    }
    if (trace)
        catn <- function(...) do.call("cat", c(lapply(list(...),
            formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width))
        text.width <- max(strwidth(legend, units = "user", cex = cex))
    else if (!is.numeric(text.width) || text.width < 0)
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1], warn.log = FALSE)
    yc <- Cex * yinch(cin[2], warn.log = FALSE)
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- max(yc, strheight(legend, units = "user", cex = cex))
    ychar <- yextra + ymax
    if (trace)
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,
            ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
        0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1)
            warning("horizontal specification overrides: Number of columns := ",
                n.leg)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    if (has.pch <- !missing(pch) && length(pch) > 0) {
        if (is.character(pch) && !is.na(pch[1]) && nchar(pch[1],
            type = "c") > 1) {
            if (length(pch) > 1)
                warning("not using pch[2..] since pch[1] has multiple chars")
            np <- nchar(pch[1], type = "c")
            pch <- substr(rep.int(pch[1], np), 1:np, 1:np)
        }
        if (!merge)
            dx.pch <- x.intersp/2 * xchar
    }
    x.off <- if (merge)
        -0.7
    else 0
    if (is.na(auto)) {
        if (xlog)
            x <- log10(x)
        if (ylog)
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1]
        top <- y[2]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust))
            xjust <- 0.5
        if (missing(yjust))
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill)
            w0 <- w0 + dx.fill
        if (has.pch && !merge)
            w0 <- w0 + dx.pch
        if (do.lines)
            w0 <- w0 + (2 + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (tw <- strwidth(title, units = "user",
            cex = cex) + 0.5 * xchar) > w) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep(inset, length.out = 2)
            insetx <- inset[1] * (usr[2] - usr[1])
            left <- switch(auto, bottomright = , topright = ,
                right = usr[2] - w - insetx, bottomleft = , left = ,
                topleft = usr[1] + insetx, bottom = , top = ,
                center = (usr[1] + usr[2] - w)/2)
            insety <- inset[2] * (usr[4] - usr[3])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3] +
                h + insety, topleft = , top = , topright = usr[4] -
                insety, left = , right = , center = (usr[3] +
                usr[4] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace)
            catn("  rect2(", left, ",", top, ", w=", w, ", h=",
                h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL, border = border.col)#added border = border.col
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
        rep.int(n.legpercol, ncol)))[1:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1:n.legpercol,
        ncol)[1:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            fill <- rep(fill, length.out = n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
                col = fill, density = density, angle = angle,
                border = box.col) #removed internal border
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines))
        col <- rep(col, length.out = n.leg)
    if (missing(lwd))
        lwd <- par("lwd")
    if (do.lines) {
        seg.len <- 2
        if (missing(lty))
            lty <- 1
        lty <- rep(lty, length.out = n.leg)
        lwd <- rep(lwd, length.out = n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0)
        if (trace)
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot)
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
                xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep(pch, length.out = n.leg)
        pt.bg <- rep(pt.bg, length.out = n.leg)
        pt.cex <- rep(pt.cex, length.out = n.leg)
        pt.lwd <- rep(pt.lwd, length.out = n.leg)
        ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
        x1 <- (if (merge)
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace)
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
                ", ...)")
        if (plot)
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
                bg = pt.bg[ok], lwd = pt.lwd[ok])
        if (!merge)
            xt <- xt + dx.pch
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title))
            text2(left + w/2, top - ymax, labels = title, adj = c(0.5,
                0), cex = cex, col = text.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex,
            col = text.col)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top),
        text = list(x = xt, y = yt)))
}

# ------------------------------------------------------------------------------

# This is not a function, per se, but a way to set up specific color pallets
# for use in the charts we use.  These pallets have been designed to create
# readable, comparable line and bar graphs with specific objectives outlined
# before each category below.

# We use this approach rather than generating them on the fly for two reasons:
# 1) fewer dependencies on libraries that don't need to be called dynamically;
# and 2) to guarantee the color used for the n-th column of data.

    # FOCUS PALETTE
    # Colorsets designed to provide focus to the data graphed as the first element.
    # This palette is best used when there is clearly an important data set for the
    # viewer to focus on, with the remaining data being secondary, tertiary, etc.
    # Later elements graphed in diminishing values of gray.  These were generated
    # with RColorBrewer, using the 8 level "grays" palette and replacing the darkest
    # with the focus color.

    # For best results, replace the highlight color with the first color of the
    # equal weighted palette from below.  This will coordinate charts with different
    # purposes.

    #Red as highlight
    redfocus = c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    #Green as highlight
    greenfocus = c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    #Blue as highlight
    bluefocus = c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    # EQUAL WEIGHT PALETTES
    # These colorsets are fine for when all of the data should be observed and
    # distinguishable on a line graph. The different numbers in the name
    # indicate the number of colors generated (six colors is probably the maximum
    # for a readable linegraph).

    # Generated with rainbow(12, s = 0.6, v = 0.75)
    rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")

    rainbow10equal = c("#BF4D4D", "#BF914D", "#A8BF4D", "#63BF4D", "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF", "#A84DBF", "#BF4D91")

    rainbow8equal = c("#BF4D4D", "#BFA34D", "#86BF4D", "#4DBF69", "#4DBFBF", "#4D69BF", "#864DBF", "#BF4DA3")

    rainbow6equal = c("#BF4D4D", "#BFBF4D", "#4DBF4D", "#4DBFBF", "#4D4DBF", "#BF4DBF")

    # Generated with package "gplots" function rich.colors(12)
    rich12equal = c("#000040", "#000093", "#0020E9", "#0076FF", "#00B8C2", "#04E466", "#49FB25", "#E7FD09", "#FEEA02", "#FFC200", "#FF8500", "#FF3300")

    rich10equal = c("#000041", "#0000A9", "#0049FF", "#00A4DE", "#03E070", "#5DFC21", "#F6F905", "#FFD701", "#FF9500", "#FF3300")

    rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")

    rich6equal = c("#000043", "#0033FF", "#01CCA4", "#BAFF12", "#FFCC00", "#FF3300")

    # Generated with package "fields" function tim.colors(12), which is said to
    # emulate the default matlab colorset
    tim12equal = c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000")

    tim10equal = c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#800000")

    tim8equal = c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", "#CFFF30", "#FF9F00", "#FF1000", "#800000")

    tim6equal = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000")

    # Generated with sort(brewer.pal(8,"Dark2")) #Dark2, Set2

    dark8equal = c("#1B9E77", "#666666", "#66A61E", "#7570B3", "#A6761D", "#D95F02", "#E6AB02", "#E7298A")

    dark6equal = c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", "#E6AB02", "#E7298A")

    set8equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F")

    set6equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#E78AC3", "#FC8D62", "#FFD92F")

    # MONOCHROME PALETTES
    # sort(brewer.pal(8,"Greens"))
    redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0")

    greenmono = c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")

    bluemono = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF")

    grey8mono = c("#000000","#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    grey6mono = c("#242424", "#494949", "#6D6D6D", "#929292", "#B6B6B6", "#DBDBDB")

# ------------------------------------------------------------------------------

# These are sets of data symbols for use as either pch or symbolset
#  e.g., chart.RiskReturnScatter(data.ts, colorset = rich8equal, symbolset =
#        c(opensymbols, closedsymbols))

    opensymbols = c(0,1,2,5,6)

    closedsymbols = c(15,16,17,18)

    fillsymbols = c(21,22,23,24,25)

    linesymbols = c(7,8,9,10,11,12,13,14)

    allsymbols = c(0:25)

# ------------------------------------------------------------------------------

# Similarly, these are event lists that we might want to annotate charts with.

    # Event lists - FOR BEST RESULTS, KEEP THESE IN ORDER
    risk.dates = c(
        "10/87",
        "02/94",
        "07/97",
        "08/98",
        "10/98",
        "07/00",
        "09/01")
    risk.labels = c(
        "Black Monday",
        "Bond Crash",
        "Asian Crisis",
        "Russian Crisis",
        "LTCM",
        "Tech Bubble",
        "Sept 11")

    equity.dates = c("01/05")
    equity.labels = c("Test Date")

    bond.dates = c()
    bond.labels = c()

    macro.dates = c()
    macro.labels = c()

    # Period beginning and endings, e.g., these are peak and trough dates from
    # http://www.nber.org/cycles.html/
    cycles.dates = list(
        c("10/26","11/27"),
        c("08/29","03/33"),
        c("05/37","06/38"),
        c("02/45","10/45"),
        c("11/48","10/49"),
        c("07/53","05/54"),
        c("08/57","04/58"),
        c("04/60","02/61"),
        c("12/69","11/70"),
        c("11/73","03/75"),
        c("01/80","07/80"),
        c("07/81","11/82"),
        c("07/90","03/91"),
        c("03/01","11/01"))

# ------------------------------------------------------------------------------

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2008/02/15 04:20:17  peter
# - parameterized box color separately from background elements
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################