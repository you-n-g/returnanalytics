`chart.RollingRegression` <-
function (Ra, Rb, width = 12, Rf = 0, attribute = c("Beta", "Alpha", "R-Squared"), main=NULL, na.pad = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    Ra = checkData(Ra, method="zoo")
    Rb = checkData(Rb, method="zoo")
    #Rf = checkDataMatrix(Rf)
    attribute=attribute[1]

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    # @todo: make an excess return function and use it here
    Ra.excess = Return.excess(Ra, Rf)
    Rb.excess = Return.excess(Rb, Rf)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        for(column.b in 1:columns.b) { # against each asset passed in as Rb
            merged.assets = merge(Ra.excess[,column.a,drop=FALSE], Rb.excess[,column.b,drop=FALSE])
            if(attribute == "Alpha")
                column.result = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) lm(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[1], by = 1, by.column = FALSE, na.pad = na.pad, align = "right")
            if(attribute == "Beta")
                column.result = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) lm(x[,1,drop=FALSE]~x[,2,drop=FALSE])$coefficients[2], by = 1, by.column = FALSE, na.pad = na.pad, align = "right")
            if(attribute == "R-Squared")
                column.result = rollapply(na.omit(merged.assets[,,drop=FALSE]), width = width, FUN= function(x) summary(lm(x[,1,drop=FALSE]~x[,2,drop=FALSE]))$r.squared, by = 1, by.column = FALSE, na.pad = na.pad, align = "right")

            # some backflips to name the single column zoo object
            column.result.tmp = xts(column.result)
            colnames(column.result.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
            column.result = xts(column.result.tmp, order.by = time(column.result))

            if(column.a == 1 & column.b == 1)
                Result.calc = column.result
            else
                Result.calc = merge(Result.calc, column.result)
        }
    }

    if(is.null(main)){
      freq = periodicity(Ra)

      switch(freq$scale,
          minute = {freq.lab = "minute"},
          hourly = {freq.lab = "hour"},
          daily = {freq.lab = "day"},
          weekly = {freq.lab = "week"},
          monthly = {freq.lab = "month"},
          quarterly = {freq.lab = "quarter"},
          yearly = {freq.lab = "year"}
      )

      main = paste("Rolling ",width,"-",freq.lab," ", attribute, sep="")
    }
    chart.TimeSeries(Result.calc, main = main, ...)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.RollingRegression.R,v 1.23 2009-10-22 03:33:36 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.22  2009-10-15 21:41:13  brian
# - updates to add automatic periodicity to chart labels, and support different frequency data
#
# Revision 1.21  2009-10-11 12:19:03  brian
# - rf to Rf
#
# Revision 1.20  2009-10-10 12:40:08  brian
# - update copyright to 2004-2009
#
# Revision 1.19  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.18  2009-10-03 05:01:09  peter
# - reversion to prior, still working, version
#
# Revision 1.16  2009-04-18 02:56:53  peter
# - argument cleanup and codoc issues
#
# Revision 1.15  2009-03-20 03:22:53  peter
# - added xts
#
# Revision 1.14  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.13  2007/10/11 03:53:00  peter
# - fixed bug for handling yearmon class dates in zoo object
#
# Revision 1.12  2007/04/14 15:01:03  brian
# - standardize Ra as first argument for asset returns
#
# Revision 1.11  2007/04/14 15:00:08  brian
# - make 'attribute' an enumerated argument
#
# Revision 1.10  2007/03/22 11:47:40  peter
# - changed legend label separator to "to"
#
# Revision 1.9  2007/03/16 03:20:51  peter
# - minor changes
#
# Revision 1.8  2007/03/15 01:15:03  brian
# - replace drop=F with drop=FALSE for R CMD check compatibility
#
# Revision 1.7  2007/03/14 22:54:13  peter
# - fixed Rf calc
#
# Revision 1.6  2007/03/14 04:53:47  peter
# - uses checkData function
# - uses zoo rollapply function
# - takes multiple assets and multiple benchmarks
# - handles unequal lengths of timeseries data
#
# Revision 1.5  2007/03/04 18:38:19  brian
# - update function definition to agree with usage using enumerated argument
#
# Revision 1.4  2007/02/07 15:45:33  peter
# - repaired graphic parameter passing
# - Rf needs a data check and testing
#
# Revision 1.3  2007/02/07 14:58:36  peter
# - added Rf and calculation of excess returns
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
