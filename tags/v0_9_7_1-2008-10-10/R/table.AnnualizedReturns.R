`table.AnnualizedReturns` <-
function (R, ci = 0.95, scale = 12, rf = 0, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Annualized Returns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of annualized returns and risk measures

    # FUNCTION:

    y = checkData(R, method = "zoo")
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")

    # Set up dimensions and labels
    columns = ncol(y)
    #rows = nrow(y)
    columnnames = colnames(y)
    #rownames = rownames(y)

    # for each column, do the following:
    for(column in 1:columns) {
        z = c(Return.annualized(y[,column,drop=FALSE], scale = scale), StdDev.annualized(y[,column,drop=FALSE], scale = scale), SharpeRatio.annualized(y[,column,drop=FALSE], scale = scale, rf = rf)) 
        znames = c("Annualized Return", "Annualized Std Dev", paste("Annualized Sharpe (rf=",base::round(mean(rf)*scale,4)*100,"%)", sep="") )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames) 
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.AnnualizedReturns.R,v 1.8 2008-08-16 03:41:14 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/08/16 14:41:09  peter
# - NA removal now handled in individual calcs
# - added checkData for rf
#
# Revision 1.5  2007/03/22 14:44:48  peter
# - uses checkData
# - eliminated firstcolumn
#
# Revision 1.4  2007/02/25 18:23:40  brian
# - change call to round() to call base::round() to fix conflict with newest fCalendar
#
# Revision 1.3  2007/02/22 22:14:46  brian
# - standardize use of 'digits' as a parameter to round()
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################