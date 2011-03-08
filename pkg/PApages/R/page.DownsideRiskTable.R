page.DownsideRiskTable <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0, width = 36, rmar=0.7, cmar=1, max.cex =1, wrap.rownames=20, wrap.colnames=10, sortby = 11, main = NULL, ...)
{ # @author Peter Carl
    if(is.null(main))
        main = "Downside Risk"

    colorset = c(rep(manager.color,length(manager.column)), rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))

    # First, create the table 
    w = t(table.DownsideRisk(R[,c(manager.column, index.columns, peer.columns),drop=FALSE], digits=4, Rf=Rf))*100

    # Sort the table by Sharpe ratio
     w.order = order(w[,sortby], decreasing=TRUE)

    # Format the resulting table
    textplot(format.df(w[w.order,], na.blank=TRUE, numeric.dollar = FALSE, cdec=rep(1,dim(w)[2])), rmar = rmar, cmar = cmar,  max.cex=max.cex, halign = "center", valign = "top", row.valign="center", wrap.rownames=wrap.rownames, wrap.colnames=wrap.colnames, col.rownames=colorset[w.order],  mar = c(0,0,3,0)+0.1)

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
# $Id: page.DownsideRiskTable.R,v 1.6 2009-11-05 22:07:37 peter Exp $
#
###############################################################################
# $Log: page.DownsideRiskTable.R,v $
# Revision 1.6  2009-11-05 22:07:37  peter
# - added margin
#
# Revision 1.5  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.4  2009-08-19 21:48:07  peter
# - fixed format.df default for numeric.dollar
#
# Revision 1.3  2009-08-19 21:35:59  peter
# - changed column margin
#
# Revision 1.2  2009-06-04 15:02:21  peter
# - added parameters for wrapping names and cex.max
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
