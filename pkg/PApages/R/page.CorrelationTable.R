page.CorrelationTable <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, filename = "workbook.pdf", manager.color = "red", peer.color = "darkgray", index.color = "orange", rf = 0, width = 36, rmar=0.7, cmar=0.7, ...)
{ # @author Peter Carl
    colorset = c(rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))
    start.row = NROW(R)-width+1
    end.row = NROW(R)
    # First, create the table 
    w = table.Correlation(R[start.row:end.row,manager.column,drop=FALSE], R[,c( index.columns, peer.columns),drop=FALSE], digits=4, rf=rf)

    # Sort the table by Sharpe ratio
     w.order = order(w[,1], decreasing=TRUE)

    # Format the resulting table
    textplot(format.df(w[w.order,], na.blank=TRUE, numeric.dollar= FALSE, cdec=rep(4,dim(w)[2])), rmar = rmar, cmar = cmar,  max.cex=.9, halign = "center", valign = "center", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=colorset[w.order])

}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.CorrelationTable.R,v 1.2 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.CorrelationTable.R,v $
# Revision 1.2  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
