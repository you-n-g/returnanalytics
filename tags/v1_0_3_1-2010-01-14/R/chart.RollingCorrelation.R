chart.RollingCorrelation <-
function (Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), na.pad = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of rolling correlation metrics in a line chart

    # FUNCTION:

    # Transform input data to a matrix
    Ra = checkData(Ra)
    Rb = checkData(Rb)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,column.b,drop=FALSE])
            column.calc = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) cor(x[,1,drop=FALSE], x[,2,drop=FALSE]), by = 1, by.column = FALSE, na.pad = na.pad, align = "right")

            # some backflips to name the single column zoo object
            column.calc.tmp = xts(column.calc)
            colnames(column.calc.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            column.calc = xts(column.calc.tmp, order.by = time(column.calc))

            if(column.a == 1 & column.b == 1)
                Result.calc = column.calc
            else
                Result.calc = merge(Result.calc, column.calc)
        }
    }

    chart.TimeSeries(Result.calc, xaxis = xaxis, col = colorset, legend.loc = legend.loc, ylim = c(-1,1), ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################