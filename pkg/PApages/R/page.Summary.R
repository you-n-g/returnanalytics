page.Summary <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, main = NULL, methods = c("ModifiedVaR", "HistoricalVaR"), width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", lwd = 2, ...)
{ # @author Peter Carl

    begin = begin[1]
    x = checkData(R, method = "zoo")
    colnames = colnames(x)
    ncols = ncol(x)

    length.column.one = length(x[,manager.column])
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

    if(ylog)
        wealth.index = TRUE

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2,3,4),nrow=4,ncol=1),heights=c(3,1.25,1.75,3), widths=1)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x[,c(manager.column,index.columns,peer.columns),drop = FALSE], main = main, xaxis = FALSE, ylab = NULL, legend.loc = NULL, event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, begin = begin, colorset = colorset, lwd = lwd, lty=linetypes, cex.axis=1, ...)

    if(!is.null(legend.loc)){
        legend(legend.loc, inset = 0.02, text.col = legend.colorset, col = legend.colorset, cex = .8, border.col = "gray", lwd = 2, bg = "white", legend = legendnames, lty=legend.linetypes, pch = legend.dottypes, pt.bg="white", pt.lwd = "1", merge = FALSE, pt.cex = 1.25)
    }

    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))
    chart.BarVaR(x[,c(manager.column,index.columns,peer.columns),drop = FALSE], main = "", xaxis = FALSE, width = width, ylab = "Monthly Return", methods = methods, event.labels = NULL, ylog=FALSE, gap = gap, colorset = colorset, lwd = lwd, cex.axis=1, ...)

    # The third row is the underwater plot
    par(mar=c(3,4,0,2))
    chart.Drawdown(x[,c(manager.column,index.columns,peer.columns),drop = FALSE], main = "", xlab = "", ylab = "From Peak", event.labels = NULL, ylog=FALSE, colorset= colorset, lwd = lwd, lty = linetypes, cex.axis=1, ...)

    par(mar=c(3,4,3,2))
    chart.RelativePerformance(x[,manager.column,drop=FALSE], x[,c(index.columns,peer.columns),drop = FALSE], main = "Relative Performance", xlab = "", ylab = "Relative Return", event.labels = NULL, ylog=FALSE, colorset= colorset[-1], lwd = lwd, lty = linetypes[-1], cex.main=0.7, cex.axis=1, ...)
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
# $Id: page.Summary.R,v 1.2 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.Summary.R,v $
# Revision 1.2  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
