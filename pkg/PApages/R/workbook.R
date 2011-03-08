book.Performance <-
function (R, manager.column = 1, peer.columns = NULL, index.columns = NULL, style.columns, filename = "workbook.pdf", manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0.04/12, width = 36, prefix = "", stepsize=12, pages = c("Summary", "Stats", "Scatter", "Performance", "Regression", "Capture", "Distribution", "Autocorrelation", "Downside", "Style"), ...)
{ # @author Peter Carl
    pdf(file=paste(prefix, filename, sep=""), height=10, width=7.5, paper="letter")

    for(page in pages){
        switch(page,
            Summary = {
                page.Summary(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf)
            },
            Stats = {
                page.SummaryTables(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf)
            },
            Scatter = {
                page.Scatter(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf, width=width, stepsize=stepsize)
            },
            Performance = {
                page.Performance(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf, width=width, na.pad=FALSE)
            },
            Regression = {
                for(i in index.columns){
                    page.Regression(R,manager.column=manager.column,peer.columns=peer.columns, index.columns=i, Rf=Rf, width=width)
                }
            },
            Capture = {
                for(i in index.columns){
                    page.Capture(R,manager.column=manager.column,peer.columns=peer.columns, index.columns=i, width=width)
                }
            },
            Distribution = {
                page.Distribution(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf)
            },
            Autocorrelation = {
                page.Autocorrelation(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf)
            },
            Downside = {
                page.DownsideRiskTable(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, Rf=Rf)
            },
            Style = {
                page.RollingStyle(R.fund=R[,manager.column], R.style=R[,style.columns], method="constrained", leverage=FALSE, width=36)
            }
        )
    }
    #page.AnnualPerformanceTable(R, manager.column=manager.column, peer.columns=peer.columns, index.columns=index.columns, rf=rf)
    dev.off()
}

books.Performance <-
function (R, peer.columns = NULL, index.columns = NULL, style.columns, filename = "workbook.pdf", manager.color = "red", peer.color = "darkgray", index.color = "orange", Rf = 0.04/12, width = 36, prefix = "", stepsize=12, pages = c("Summary", "Scatter", "Performance", "Regression", "Distribution", "Autocorrelation", "Downside", "Style"), ...)
{  # @author Peter Carl
    columnnames = colnames(R)
    
    for(i in peer.columns) {
        filename = paste(prefix, columnnames[i],".pdf", sep="")
        print(paste(i,columnnames[i], sep=" "))
        book.Performance(R, manager.column = i, peer.columns = peer.columns[-i], index.columns = index.columns, style.columns = style.columns, filename = filename, manager.color = manager.color, peer.color = peer.color, index.color = index.color, Rf = Rf, width = width, prefix = prefix, stepsize = stepsize, pages = pages, ...)
    }
}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: workbook.R,v 1.3 2009-10-08 20:05:39 peter Exp $
#
###############################################################################
# $Log: workbook.R,v $
# Revision 1.3  2009-10-08 20:05:39  peter
# - changed parameter names to match PA versions
#
# Revision 1.2  2009-06-02 02:46:46  peter
# - fixed missing bracket
#
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
