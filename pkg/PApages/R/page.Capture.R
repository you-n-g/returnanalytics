page.Capture <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", width = 36, legend.loc = NULL, rmar=0.7, cmar=0.7, ...)
{ # @author Peter Carl

    x = checkData(R, method = "xts")
    colnames = colnames(x)
    ncols = ncol(x)

    length.column.one = length(x[,manager.column])
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,manager.column])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,]

    colorset = c(rep(manager.color,length(manager.column)), rep(peer.color,length(peer.columns)))
    legend.colorset = c(rep(manager.color, length(manager.column)), peer.color)

    linetypes = c(rep(1, length(manager.column)), rep(1, length(peer.columns)))
    legend.linetypes = c(rep(1, length(manager.column)), 1)

    dottypes = c(rep(1, length(manager.column)), rep(1, length(peer.columns)))
    legend.dottypes = c(rep(1, length(manager.column)),NULL)

    if(ncols > 1){
        legend.loc = legend.loc
        legendnames = c(colnames(x[, manager.column, drop = FALSE]),"Peer group", colnames(x[, index.columns, drop = FALSE]))
    }
    else
        legend.loc = NULL

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2,3,4),nrow=2,byrow=TRUE), height = c(1,1.5), width = 1)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first three rows are the rolling window chart of the Excess returns of the index versus the benchmark
    par(mar=c(5,4,4,2)+0.1, cex=0.8)
    chart.CaptureRatios(x[, c(manager.column, peer.columns), drop = FALSE], x[,index.columns[1], drop = FALSE], main = paste("Capture vs ",colnames(x[, index.columns,drop=FALSE])[1],sep=""), legend.loc=NULL, colorset = colorset, benchmark.color = index.color, ...)

    par(mar=c(5,4,4,2)+0.1, cex=0.8)
    chart.CaptureRatios(last(x[, c(manager.column, peer.columns), drop = FALSE],width), last(x[,index.columns[1], drop = FALSE], width), main = paste("Trailing ",width,"-Month Capture vs ",colnames(x[, index.columns,drop=FALSE])[1],sep=""), colorset = colorset, benchmark.color = index.color, ...)

    par(mar=c(3,4,4,2)+0.1)
    w=table.UpDownRatios(x[, c(manager.column, peer.columns), drop = FALSE], x[,index.columns[1], drop = FALSE])
    rownames(w) = colnames(x[, c(manager.column, peer.columns), drop = FALSE])

    # Sort the table by Up Capture ratio
     w.order = order(w[,1], decreasing=TRUE)

    # Format the resulting table
    textplot(format.df(w[w.order,], na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(2,dim(w)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=8, col.rownames=colorset[w.order])

    par(mar=c(3,4,4,2)+0.1)
    w=table.UpDownRatios(last(x[, c(manager.column, peer.columns), drop = FALSE], width), last(x[,index.columns[1], drop = FALSE], width))
    rownames(w) = colnames(x[, c(manager.column, peer.columns), drop = FALSE])

    # Sort the table by Up Capture ratio
     w.order = order(w[,1], decreasing=TRUE)

    # Format the resulting table
    textplot(format.df(w[w.order,], na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(2,dim(w)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=8, col.rownames=colorset[w.order])
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
# $Id: page.Capture.R,v 1.3 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.Capture.R,v $
# Revision 1.3  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.2  2009-08-19 21:54:24  peter
# - fixed format.df default for numeric.dollar
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
