page.Scatter <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, method = c("ModifiedVaR","VaR","StdDev"), event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", lwd = 1, stepsize = 12, width = 36, ...)
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
    nrows = nrow(x)

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

    # First, we lay out the graphic as a two row, two column format
    layout(matrix(c(1,1,2,3),nrow=2,ncol=2,byrow=T),heights=c(6,3),widths=1)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the trailing 36-month risk-return scatter chart
    par(mar=c(5, 4, 4, 2))
    chart.RiskReturnScatter(x[(nrows-width+1):nrows,c(manager.column,index.columns,peer.columns),drop = FALSE], main =paste("Performance During Trailing ",width,"-Months",sep=""),  legend.loc = NULL, colorset = colorset, lwd = lwd, lty=linetypes, Rf=Rf, symbolset = dottypes, add.names=TRUE, cex.axis=1, cex.main=1, cex.lab= 1, ...)

    # The second row contains complement charts; the first is "inception to date" of 
    # the same chart as above.
    par(mar=c(5,4,2,2))
    chart.RiskReturnScatter(x[,c(manager.column,index.columns,peer.columns),drop = FALSE], Rf = Rf, main= paste("Since Inception of ",colnames[manager.column],sep=""),add.names=FALSE, colorset = colorset, symbolset = dottypes, cex.axis=0.8, cex.main=0.8, cex.lab=0.8, ...)
    
    # The second chart in the second row shows the snail-trail chart of the 36-month 
    # trailing performance for the manager.
    par(mar=c(5,4,2,2))
    chart.SnailTrail(x[,c(manager.column,index.columns[1]),drop = FALSE], main=paste("Trailing ",width,"-Months Calc\'d ",stepsize," Months",sep=""), stepsize = stepsize, width = width, Rf=Rf, add.names="lastonly", colorset = colorset, symbolset = dottypes, cex.axis=0.8, cex.main=0.8, cex.lab=0.8, cex.text = 0.7,...)

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
# $Id: page.Scatter.R,v 1.2 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.Scatter.R,v $
# Revision 1.2  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
