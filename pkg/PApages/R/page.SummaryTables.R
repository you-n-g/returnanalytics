page.SummaryTables <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, filename = "workbook.pdf", manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, width = 36, rmar=0.7, cmar=1, ...)
{ # @author Peter Carl

    # This page is focused on only the statistics for the manager, and is 
    # not attempting any comparison to peers.

    # Three or more tables: monthly returns at the top, time window analysis in middle, 
    # benchmark analysis at bottom, risk/return metrics (monthly/annual)? 
    # drawdown analysis?

    x = checkData(R)
    colnames = colnames(x)
    ncols = ncol(x)

    length.column.one = length(x[,manager.column])
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,manager.column])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,]

    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,1,2,3,4,4), ncol=2,byrow=TRUE),height=c(3,5,1),width=1)
layout.show(4)
    calendar = table.CalendarReturns(x[,c(manager.column, index.columns), drop=FALSE])

    calendar.order = order(rownames(calendar), decreasing=TRUE)

    textplot(format.df(t(calendar[calendar.order,]), na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(1,dim(calendar)[1])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=10, wrap.colnames=10)

    rollstats = table.MgrRollingStats(x[,c(manager.column, index.columns), drop=FALSE], FUNCS = c("Return.annualized", "sd.annualized", "SharpeRatio.annualized", "maxDrawdown", "UpsidePotentialRatio"), funcs.names = c("Annualized Return", "Annualized Std Dev", "Annualized Sharpe", "Maximum Drawdown", "Upside Potential Ratio"))
    textplot(format.df(rollstats, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(4,dim(rollstats)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=10)

stats= table.Stats(x[,c(manager.column, index.columns), drop=FALSE])
textplot(format.df(stats, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(4,dim(stats)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=10)


#     benchmarks = table.CAPM(x[,c(manager.column), drop=FALSE], x[,c(index.columns), drop=FALSE], Rf=Rf)
#     colnames(benchmarks) = colnames(x[,c(index.columns), drop=FALSE])
# @todo: order this table by R-squared?
#     textplot(format.df(t(benchmarks), na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(4,dim(t(benchmarks))[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=10)

#     colorset = c(rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))
#     start.row = NROW(R)-width+1
#     end.row = NROW(R)
#     # First, create the table 
#     w = table.Correlation(R[start.row:end.row,manager.column,drop=FALSE], R[,c( index.columns, peer.columns),drop=FALSE], digits=4, rf=rf)
# 
#     # Sort the table by Sharpe ratio
#      w.order = order(w[,1], decreasing=TRUE)
# 
#     # Format the resulting table
#     textplot(format.df(w[w.order,], na.blank=TRUE, cdec=rep(4,dim(w)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=colorset[w.order])

}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: $
#
###############################################################################
# $Log: $
