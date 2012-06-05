page.HigherMomentsTable <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, filename = "workbook.pdf", manager.color = "red", peer.color = "darkgray", index.color = "orange", rf = 0, width = 36, rmar=0.7, cmar=0.7, max.cex =1, wrap.rownames=40, wrap.colnames=10, ...)
{ # @author Peter Carl
    colorset = c(rep(manager.color,length(manager.column)), rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))

    # First, create the table 
    w = t(table.HigherMoments(R[,c(manager.column, index.columns, peer.columns),drop=FALSE], R[,index.columns[1],drop=FALSE], digits=4))[,c(3,4,5)]

    # Sort the table by Sharpe ratio
    w.order = order(w[,2], decreasing=FALSE)

    # Format the resulting table
    textplot(format.df(w[w.order,], na.blank=TRUE, numeric.dollar =FALSE, cdec=rep(4,dim(w)[2])), rmar = rmar, cmar = cmar,  max.cex=max.cex, halign = "center", valign = "center", row.valign="center", wrap.rownames=wrap.rownames, wrap.colnames=wrap.colnames, col.rownames=colorset[w.order])

}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.HigherMomentsTable.R,v 1.4 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.HigherMomentsTable.R,v $
# Revision 1.4  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.3  2009-08-19 21:50:35  peter
# - fixed format.df default for numeric.dollar
#
# Revision 1.2  2009-06-04 14:52:03  peter
# - parameterized wrap for row and col names
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
