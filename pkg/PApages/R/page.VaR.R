page.VaR <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", rf = 0, event.lines = NULL, event.labels = NULL, legend.loc="topleft", lwd = 2, width = 0, gap=36, main=NULL, method=c("HistoricalVaR","ModifiedVaR","GaussianVaR"),...)
{ # @author Peter Carl

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

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,1,2,2,3,4),nrow=3,byrow=TRUE), heights = c(1,1), widths = 1)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first three rows are the rolling window chart of the Excess returns of the index versus the benchmark
    par(mar=c(3,4,4,2)+0.1)
    chart.BarVaR(x[,c(manager.column,index.columns,peer.columns),drop = FALSE], main = "Comparing VaR to Peers", width = width, ylab = "Monthly Return", methods = method[2], event.labels = NULL, ylog=FALSE, gap = gap, colorset = colorset, lwd = lwd, cex.axis=1, all=TRUE, lty=linetypes, xlab="", ...)
    par(mar=c(3,4,4,2)+0.1)
    chart.BarVaR(x[,c(manager.column,index.columns,peer.columns),drop = FALSE], main = "Comparing Methods", width = width, ylab = "Monthly Return", methods = method, event.labels = NULL, ylog=FALSE, gap = gap, colorset = colorset, lwd = lwd, cex.axis=1, clean="boudt", show.clean=TRUE, xlab="", ...)

    par(mar=c(5,4,4,2)+0.1)
    chart.VaRSensitivity(x[,manager.column,drop=FALSE], lwd=2, main="Raw", ylim=c(-.08,0))
    par(mar=c(5,4,4,2)+0.1)
    chart.VaRSensitivity(x[,manager.column,drop=FALSE], lwd=2, clean="boudt", main="Cleaned", ylim=c(-.08,0))

}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.VaR.R,v 1.1 2009-06-02 02:32:06 peter Exp $
#
###############################################################################
# $Log: page.VaR.R,v $
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
