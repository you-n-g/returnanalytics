page.Performance <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, main = NULL, method = c("ModifiedVaR","VaR","StdDev"), width = 36, legend.loc="topleft", lwd = 2, na.pad = TRUE, ...)
{ # @author Peter Carl

    x = checkData(R, method = "zoo")
    colnames = colnames(x)
    ncols = ncol(x)

    length.column.one = length(x[,manager.column])
# find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,manager.column])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,]

    colorset = c(rep(manager.color,length(manager.column)), rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))
    legend.colorset = c(rep(manager.color, length(manager.column)), peer.color, rep(index.color, length(index.columns)))

    linetypes = c(rep(1, length(manager.column)), 1:length(index.columns), rep(1, length(peer.columns)))
    legend.linetypes = c(rep(1, length(manager.column)), 1, 1:length(index.columns))

    dottypes = c(rep(16, length(manager.column)), closedsymbols[1:length(index.columns)], rep(1, length(peer.columns)))
    legend.dottypes = c(rep(16, length(manager.column)),1, closedsymbols[1:length(index.columns)])

    if(ncols > 1){
        legend.loc = legend.loc
        legendnames = c(colnames(x[, manager.column, drop = FALSE]),"Peer group", colnames(x[, index.columns, drop = FALSE]))
    }
    else
        legend.loc = NULL

    if(is.null(main))
        main = paste(colnames[manager.column],"Performance", sep=" ")

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2,3,4)),heights=c(2.2,1.6,2.2,3),widths=1)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.RollingPerformance(x[, c(manager.column, index.columns, peer.columns), drop = FALSE], width = width, FUN = "Return.annualized", xaxis = FALSE, legend.loc=NULL, colorset = colorset, lwd = lwd, na.pad = na.pad, ylab = "Ann. Return", lty = linetypes, cex.axis=1, ...)

    if(!is.null(legend.loc)){
        legend(legend.loc, inset = 0.02, text.col = legend.colorset, col = legend.colorset, cex = .8, border.col = "gray", lwd = 2, bg = "white", legend = legendnames, lty=legend.linetypes, pch = legend.dottypes, pt.bg="white", pt.lwd = "1", merge = FALSE, pt.cex = 1.25)
    }

    par(mar=c(1,4,0,2))
    chart.RollingPerformance(x[, c(manager.column, index.columns, peer.columns), drop = FALSE], FUN = "StdDev.annualized", width = width, main = "", xaxis = FALSE, event.labels = NULL, colorset = colorset, lwd = 2, na.pad = na.pad, ylab = "Ann. Std. Dev.", lty = linetypes, cex.axis=1, ...)

    par(mar=c(5,4,0,2))
    chart.RollingPerformance(x[, c(manager.column, index.columns, peer.columns), drop = FALSE], FUN = "SharpeRatio.annualized", width = width, Rf = Rf, main = "", event.labels = NULL, colorset = colorset, lwd = 2, xlab="", na.pad = na.pad, ylab = "Ann. Sharpe", lty = linetypes, cex.axis=1, ...)

    par(mar=c(3,4,4,2))
    chart.RollingCorrelation(x[,manager.column,drop=FALSE], x[,c(index.columns,peer.columns),drop = FALSE], xlab = "", event.labels = NULL, ylog=FALSE, colorset= colorset[-1], lwd = lwd, lty = linetypes[-1], cex.main=1, width = width, main = paste("Rolling ", width, "-Month Correlation to ", colnames[manager.column], sep=""), ylab="Correlation", cex.axis=1, ...)
    par(op)
}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.Performance.R,v 1.3 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.Performance.R,v $
# Revision 1.3  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.2  2009-06-03 19:52:07  peter
# - removed begin parameter
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
