page.AutocorrelationTable <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, manager.color = "red", peer.color = "darkgray", index.color = "orange", main = NULL, rmar=0.7, cmar=0.7, max.cex =1, mar = c(5,4,4,2)+.1)
{ # @author Peter Carl

    x = checkData(R, method = "zoo")
    colnames = colnames(x)
    ncols = ncol(x)

    if(!is.null(manager.column)){
        length.column.one = length(x[,manager.column])
        start.row = 1
        start.index = 0
        while(is.na(x[start.row,manager.column])){
            start.row = start.row + 1
        }
        x = x[start.row:length.column.one,]
    }

    colorset = c(rep(manager.color,length(manager.column)), rep(index.color, length(index.columns)), rep(peer.color,length(peer.columns)))

    w = t(table.Autocorrelation(x[, c(manager.column, index.columns, peer.columns), drop = FALSE]))

    # sort by p-value
    w.order = order(w[,7], decreasing=FALSE)

    if(is.null(main)) {
        main = "Autocorrelation"
    }
    title(main = main)

    textplot(format.df(w[w.order,],na.blank=TRUE, numeric.dollar=FALSE, rdec=c(rep(4,dim(w)[1])), col.just=rep("nc",dim(w)[2])), rmar = rmar, cmar = cmar, max.cex=max.cex, halign = "center", valign = "center", row.valign="center", wrap.rownames=50, wrap.colnames=10, col.rownames=colorset[w.order])

}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.AutocorrelationTable.R,v 1.7 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: page.AutocorrelationTable.R,v $
# Revision 1.7  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.6  2009-08-19 21:56:22  peter
# - added title to table
#
# Revision 1.5  2009-08-19 21:49:27  peter
# - fixed format.df default for numeric.dollar
#
# Revision 1.4  2009-06-04 14:33:03  peter
# - changed calculation order to match colorset
#
# Revision 1.3  2009-06-04 14:07:55  peter
# - modified sort order and row label length
#
# Revision 1.2  2009-06-04 14:05:03  peter
# - removed begin parameter
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
