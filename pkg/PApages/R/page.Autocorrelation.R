page.Autocorrelation <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", main = NULL, lwd = 2, rmar=0.7, cmar=0.9, max.cex =1, maxlag = NULL, elementcolor="darkgray", ...)
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

    if(is.null(main))
        main = colnames(x[, manager.column, drop = FALSE])

    data = checkData(na.omit(x[,manager.column]), method="vector")

    num = length(data)
    if (is.null(maxlag)) 
        maxlag = ceiling(10 + sqrt(num))
    ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
    PACF = pacf(data, maxlag, plot = FALSE)$acf
    Lag = 1:length(ACF)/frequency(data)
    minA = min(ACF)
    minP = min(PACF)
    U = 2/sqrt(num)
    L = -U
    minu = min(minA, minP, L) - .01

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2,3),nrow=3,ncol=1),heights=c(1,1,2),widths=1)
    
    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # ACF chart
    par(mar=c(0.5,4,4,2) + 0.1)
    plot(Lag, ACF, type = "h", ylim = c(minu,1), main = main, axes = FALSE, lwd = lwd, lend="butt", ...)
    box(col=elementcolor)
    axis(2, col = elementcolor, cex.axis = 1)
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))

    # PACF chart
    par(mar=c(4,4,0.5,2)+ 0.1)
    plot(Lag, PACF, type = "h", ylim = c(minu,1), axes = FALSE, lwd = lwd, lend="butt", ...)
    box(col=elementcolor)
    axis(1, col = elementcolor, cex.axis = 1)
    axis(2, col = elementcolor, cex.axis = 1)
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))

    # Autocorrelation table, sorted by Q
    par(mar = c(5,4,4,2)+.1)
    w = t(table.Autocorrelation(x[, c(manager.column, index.columns, peer.columns), drop = FALSE]))
    # sort by p-value
    w.order = order(w[,7], decreasing=FALSE)


    textplot(format.df(w[w.order,],na.blank=TRUE,numericdollar=FALSE,rdec=c(rep(4,dim(w)[1])), col.just=rep("nc",dim(w)[2])), rmar = rmar, cmar = cmar, max.cex=max.cex, halign = "center", valign = "center", row.valign="center", wrap.rownames=50, wrap.colnames=10, col.rownames=colorset[w.order])

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
# $Id: page.Autocorrelation.R,v 1.2 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.Autocorrelation.R,v $
# Revision 1.2  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
