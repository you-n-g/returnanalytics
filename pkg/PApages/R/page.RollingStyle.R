page.RollingStyle <-
function (R.fund, R.style, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, selection = "none", width = 12, main = paste(colnames(R.fund)[1]," Rolling ", width ,"-Month Style Weights", sep=""), space = 0, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # R-Squared could deliver adjusted R-Squared if we wanted

    # FUNCTION:

    # Transform input data to a data frame
    R.fund = checkData(R.fund[,1,drop=FALSE], method = "zoo")
    R.style = checkData(R.style, method = "zoo")

    method = method[1]

    # Get dimensions and labels
    columns.fund = ncol(R.fund)
    columns.style = ncol(R.style)
    columnnames.fund = colnames(R.fund)
    columnnames.style = colnames(R.style)

    # Calculate
    merged.assets = na.omit(merge(R.fund, R.style))
    result = rollapply(data = merged.assets, FUN= function(x) {t(style.fit(R.fund = x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = method, leverage = leverage, selection = selection)$weights)}, width = width, by = 1, by.column = FALSE, na.pad = FALSE, align = "right")
    # I hate to do this, but until there's a better sol'n
    fit = rollapply(data = merged.assets, FUN= function(x) {t(style.fit(R.fund = x[,1,drop=FALSE], R.style = x[,-1,drop=FALSE], method = method, leverage = leverage, selection = selection)$R.squared)}, width = width, by = 1, by.column = FALSE, na.pad = FALSE, align = "right")

#     weights = result$weights
#     fit = result$R.squared

    colnames(result) = columnnames.style
    rows = nrow(result)
# Create a page of bar chart panels (using chart.Bar) that are in a single column.
# Option to add a "total" exposure chart, which shows the positive and negative "gross" exposures in bars
# and the net exposure in an overlaid line
# Option to add an "R.squared" panel that shows the R-squared value through time in a line chart

# Establish common ylim across the bar charts
# Title at the top of the page
# x-axis in the bottom bar chart and the optional charts? or just in the bottom chart
# each bar chart title labeled with the factor name, left justified
# each panel may have a different color from colorset, but default set to the same color across all
    ymax = max(c(1,result))
    ymin = min(c(-1,result))
    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1
    op <- par(oma = c(2,0,4,0), mar=c(0,4,0,4))
    layout(matrix(c(1:columns.style, columns.style+1, columns.style+2), ncol = 1, byrow = TRUE))

    for(i in 1:columns.style){
        if(even(i))
            yaxis.right=TRUE
        else
            yaxis.right=FALSE
        chart.TimeSeries(result[,i,drop=F], type = "h", lend="butt", xaxis=FALSE, main="", ylab=colnames(result)[i], ylim = c(ymin,ymax), yaxis.right=yaxis.right, ...)
    }

# Add one more panel here
# Sum the positive weights through time (bar)
    # Brute force solution for plotting negative values in the bar charts:
    positives = result
    for(column in 1:ncol(result)){
        for(row in 1:nrow(result)){ 
            positives[row,column]=max(0,result[row,column])
        }
    }

    negatives = result
    for(column in 1:ncol(result)){
        for(row in 1:nrow(result)){ 
            negatives[row,column]=min(0,result[row,column])
        }
    }
    sumpositives = zoo(apply(positives,1,sum), order.by=index(positives))
    sumnegatives = zoo(apply(negatives,1,sum), order.by=index(negatives))
    net = apply(result,1,sum)
    if(even(columns.style+1))
        yaxis.right=TRUE
    else
        yaxis.right=FALSE
    chart.TimeSeries(cbind(sumpositives,sumnegatives), type = "h", lend="butt", xaxis=FALSE, main="", ylab="Total", yaxis.right=yaxis.right, ...)
#     chart.TimeSeries(cbind(sumpositives,sumnegatives), type="h", lend="butt", xaxis=FALSE, main="", ylab="Total", ...)
    lines(1:rows, net)
# Sum the negative weights through time (bar)
# Sum the weights through time (line)

    if(even(columns.style+2))
        yaxis.right=TRUE
    else
        yaxis.right=FALSE
    chart.TimeSeries(fit, type = "l", xaxis=TRUE, main="", ylab="AdjR^2", ylim = c(0,1), yaxis.right=yaxis.right, ...)

    mtext(main,
        side = 3, outer = TRUE, 
        font = 2, cex = 1.2, line=1)
    par(op)
    
}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.RollingStyle.R,v 1.1 2009-06-02 02:32:06 peter Exp $
#
###############################################################################
# $Log: page.RollingStyle.R,v $
# Revision 1.1  2009-06-02 02:32:06  peter
# - initial commit of package
#
