page.Distribution <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", rf = 0, event.lines = NULL, event.labels = NULL, legend.loc="topleft", lwd = 1, width = 36, main=NULL,...)
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

    # Calculate risk lines
    hVaR = quantile(as.vector(x[, c(manager.column), drop = FALSE]), probs=.01, na.rm = TRUE)[[1]]
    pVaR = -VaR.traditional(x[, c(manager.column), drop = FALSE], p=0.99)
    mVaR = -VaR.CornishFisher(x[, c(manager.column), drop = FALSE], p=0.99)
    library(sn)
    fit.st = st.mle(y=na.omit(x[, c(manager.column), drop = FALSE]))
    stVaR = qst(.01, location = fit.st$dp[[1]], scale = fit.st$dp[[2]], shape = fit.st$dp[[3]], df=fit.st$dp[[4]], log = FALSE)
require("fBasics")
    fit.stable = stableFit(as.vector(na.omit(x[,c(manager.column)])),doplot=FALSE)
    spVaR = qstable(.01,alpha = fit.stable@fit$estimate[[1]], beta = fit.stable@fit$estimate[[2]], gamma = fit.stable@fit$estimate[[3]], delta = fit.stable@fit$estimate[[4]], pm = 0)
    detach('package:fBasics', unload=TRUE)
    detach('package:timeSeries', unload=TRUE)
    detach('package:timeDate', unload=TRUE)

    op <- par(no.readonly=TRUE)
    # First, we lay out the graphic as a two row, two column format
    layout(matrix(c(1,2,3,4,5,6),ncol=2,byrow=TRUE), height = c(1,1,1), width = 1)

    par(cex = 0.8)
    # Panel 1, Distribution of historical data, risk measures, normal fit
    chart.Histogram(x[, c(manager.column), drop = FALSE], probability=TRUE, show.outliers=TRUE, main="Gaussian", note.lines = c(hVaR, pVaR, mVaR, stVaR, spVaR), note.labels = c("Historical VaR", "Parametric VaR", "Modified VaR", "", ""), note.color=c("darkblue", "darkgray", "darkgray", "darkgray", "darkgray"), methods=c("add.density","add.normal"), cex.axis=0.8, cex=.8)

    par(cex = 0.8)
    # Panel 2, QQ Plot using Normal distribution
    chart.QQPlot(x[, c(manager.column), drop = FALSE], main = "QQ Fit To Gaussian", distribution = 'norm', envelope=0.99, xlab = "Normal Quantiles")#, cex.axis=0.8, cex=.8)

    par(cex = 0.8)
    # Panel 3, ECDF?
    chart.Histogram(x[, c(manager.column), drop = FALSE], probability=T, show.outliers=T, main="Skew-T", note.lines = c(hVaR, pVaR, mVaR, stVaR, spVaR), note.labels = c("", "", "", "Skew-T VaR", ""), note.color=c("darkgray", "darkgray", "darkgray", "darkblue", "darkgray"), methods="add.sst", cex.axis=0.8, cex=.8)

    # chart.ECDF(x[, c(manager.column), drop = FALSE], main = "ECDF", lwd = 2)

    par(cex = 0.8)
    # Panel 4, Boxplot?
    chart.QQPlot(x[, c(manager.column), drop = FALSE], main = "QQ Fit To Skew-T", envelope=0.99, distribution = 'st', location = fit.st$dp[[1]], scale = fit.st$dp[[2]], shape = fit.st$dp[[3]], df=fit.st$dp[[4]], log = FALSE, xlab = "Skew-T Quantiles")#, cex.axis=0.8, cex=.8)
        # chart.Boxplot(x[, c(manager.column, peer.columns, index.columns), drop = FALSE], main = "Boxplot", names=F)

     require(fBasics)
#     library(zoo)

    par(cex = 0.8)
    chart.Histogram(x[, c(manager.column), drop = FALSE], probability=T, show.outliers=T, main="Stable", note.lines = c(hVaR, pVaR, mVaR, stVaR, spVaR), note.labels = c("", "", "", "", "Stable VaR"), note.color=c("darkgray", "darkgray", "darkgray", "darkgray", "darkblue"), methods="add.stable", cex.axis=0.8, cex=.8)
#        chart.ACF(x[, c(manager.column, peer.columns, index.columns), drop = FALSE], main = "Autocorrelation")

    par(cex = 0.8)
    chart.QQPlot(x[, c(manager.column), drop = FALSE], main = "QQ Fit To Stable", envelope=0.99, distribution = 'stable', alpha = fit.stable@fit$estimate[[1]], beta = fit.stable@fit$estimate[[2]], gamma = fit.stable@fit$estimate[[3]], delta = fit.stable@fit$estimate[[4]], pm = 0)#, cex.axis=0.8, cex=.8)
#        textplot(t(table.Autocorrelation(x[, c(manager.column, peer.columns, index.columns), drop = FALSE])))

# ADD BOXPLOTS: RANKED BY MEAN, RANKED BY VARIANCE
# ADD ANOTHER DISTRIBUTION, CAUCHY?
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
# $Id: page.Distribution.R,v 1.1 2009-06-02 02:32:06 peter Exp $
#
###############################################################################
# $Log: page.Distribution.R,v $
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
