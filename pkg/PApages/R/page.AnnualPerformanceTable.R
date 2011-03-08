page.AnnualPerformanceTable <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, width = 36, rmar=0.7, cmar=1, main = NULL, ...)
{ # @author Peter Carl

    if(is.null(main))
        main = "Annual Performance"

    colorset = c(rep(manager.color,length(manager.column)), rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))

    # First, create the table of calendar returns without the months
    w = t(table.CalendarReturns(R[,c(manager.column, index.columns, peer.columns),drop=FALSE], digits=4))[-1:-12,]

    # Next, calculate the annualized returns and Sharpe ratio
    y= t(table.AnnualizedReturns(R[,c(manager.column, index.columns, peer.columns),drop=FALSE], Rf=Rf))
    y[,1:2]=y[,1:2]*100
    colnames(y) = c("Ann Return", "Ann Std Dev", paste("Sharpe (Rf=", base::round(mean(Rf) * 12, 4) * 100, "%)", sep = ""))

    # Finally, calc worst draw down
    WDD = apply(R[,c(manager.column, index.columns, peer.columns),drop=FALSE], FUN=maxDrawdown, MARGIN=2)
    WDD = WDD*100

    # Merge the three tables
    z= cbind(w,y,WDD)

    # Sort the table by Sharpe ratio
    z.order = order(y[,1], decreasing=TRUE)

    # Format the resulting table
    textplot(format.df(z[z.order,], na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(1,dim(z)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=colorset[z.order], mar = c(0,0,3,0)+0.1)

    title(main=main)
}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.AnnualPerformanceTable.R,v 1.5 2009-11-05 22:05:11 peter Exp $
#
###############################################################################
# $Log: page.AnnualPerformanceTable.R,v $
# Revision 1.5  2009-11-05 22:05:11  peter
# - changed rf to Rf
# - modified margin
#
# Revision 1.4  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.3  2009-08-19 21:58:16  peter
# - adjusted table margin
#
# Revision 1.2  2009-08-19 21:42:11  peter
# - fixed format.df default change to numeric.dollar
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
