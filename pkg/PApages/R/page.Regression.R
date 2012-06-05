page.Regression <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, event.lines = NULL, event.labels = NULL, legend.loc="topleft", lwd = 1, width = 36, main=NULL,...)
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

    nrows = nrow(x)

    colorset = c(rep(manager.color,length(manager.column)), rep(peer.color,length(peer.columns)))
    legend.colorset = c(rep(manager.color, length(manager.column)), peer.color)

    linetypes = c(rep(1, length(manager.column)), rep(1, length(peer.columns)))
    legend.linetypes = c(rep(1, length(manager.column)), 1)

    dottypes = c(rep(1, length(manager.column)), rep(1, length(peer.columns)))
    legend.dottypes = c(rep(1, length(manager.column)),NULL)

    if(ncols > 1){
        legend.loc = legend.loc
        legendnames = c(colnames(x[, manager.column, drop = FALSE]),"Peer group")
    }
    else
        legend.loc = NULL

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,1,2,2,3,3,4,5),ncol=2,byrow=TRUE), heights = c(2.2,1.6,2.2,3), widths = 1)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first three rows are the rolling window chart of the Excess returns of the index versus the benchmark

    par(mar=c(1,4,4,2))
    if(is.null(main)){
         main = paste("Rolling ",width,"-Month Regression",sep="")
    }

    chart.RollingRegression(x[, c(manager.column, peer.columns), drop = FALSE], x[,index.columns[1], drop = FALSE], width = width, Rf = Rf, attribute = "Alpha", xaxis = FALSE, main = paste("Rolling ",width,"-Month Regression versus ",colnames(x[, index.columns,drop=FALSE])[1],sep=""), ylab = "Alpha", legend.loc=NULL, event.labels = event.labels, colorset = colorset, lwd = 2, na.pad =FALSE, cex.axis=1,...)

    if(!is.null(legend.loc)){
        legend(legend.loc, inset = 0.02, text.col = legend.colorset, col = legend.colorset, cex = .8, border.col = "gray", lwd = 2, bg = "white", legend = legendnames, lty=legend.linetypes, pch = legend.dottypes, pt.bg="white", pt.lwd = "1", merge = FALSE, pt.cex = 1.25)
    }

    par(mar=c(1,4,0,2))
    chart.RollingRegression(x[, c(manager.column, peer.columns), drop = FALSE], x[,index.columns[1], drop = FALSE], width = width, Rf = Rf, attribute = "Beta", main = "", ylab = "Beta", xaxis = FALSE, event.labels = NULL, colorset = colorset, lwd = 2, na.pad =FALSE, cex.axis=1,...)

    par(mar=c(5,4,0,2))
    chart.RollingRegression(x[, c(manager.column, peer.columns), drop = FALSE], x[,index.columns[1], drop = FALSE], width = width, Rf = Rf, attribute = "R-Squared", main = "", ylab = "R-Squared", event.labels = NULL, colorset = colorset, lwd = 2, xlab="", na.pad =FALSE, cex.axis=1,...)

    # The second row contains complement charts; the first is "inception to date" of 
    # the same chart as above.
    par(mar=c(5,4,2,2))
    chart.Regression(x[, manager.column, drop = FALSE], x[,index.columns[1], drop = FALSE], Rf = Rf, excess.returns=TRUE, colorset = colorset, symbolset = 1, cex=1, cex.axis=0.8, cex.main=0.8, cex.lab=0.8, fit = c("loess", "linear"), main = "From Inception", ...)
    
    # The second chart in the second row shows the snail-trail chart of the 36-month 
    # trailing performance for the manager.
    par(mar=c(5,4,2,2))
    chart.Regression(x[(nrows-width+1):nrows, manager.column, drop = FALSE], x[,index.columns[1], drop = FALSE], Rf = Rf, excess.returns = TRUE, colorset = colorset, symbolset = 1, cex=1, cex.axis=0.8, cex.main=0.8, cex.lab=0.8, fit = c("loess", "linear"), main = paste("Trailing ",width,"-Months",sep=""), ...)

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
# $Id: page.Regression.R,v 1.2 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.Regression.R,v $
# Revision 1.2  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
