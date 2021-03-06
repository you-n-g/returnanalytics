`checkDataVector` <-
function (R, na.rm = TRUE, quiet = TRUE, ...)
{ # @author Peter Carl

    # Description:

    # This function was created to make the different kinds of data classes
    # at least _seem_ more fungible.  It allows the user to pass in a data
    # object without being concerned that the function requires a matrix,
    # data.frame, or timeSeries object.  By using this, the function "knows"
    # what data format it has to work with.

    # Inputs:
    # R: the data provided.
    # na.rm: default behavior is to remove NA's
    # quiet: if false, it will throw warnings when errors are noticed

    # Outputs:
    # Produces a numeric vector or an NA and any associated warnings.

    # FUNCTION:

    y = checkDataMatrix(R)
    x = as.vector(y)

    # First, we'll check to see if we have more than one column.
    if (NCOL(x) > 1) {
        if(!quiet)
            warning("The data provided is not a vector or univariate time series.  Used only the first column")
        x = x[,1]
    }

    # Second, we'll hunt for NA's and remove them if required
    if (any(is.na(x))) {
        if(na.rm) {
            # Try to remove any NA's
            x = x[!is.na(x)]
            if(!quiet){
                warning("The following slots have NAs.")
                warning(paste(x@na.removed," "))
            }
        }
        else {
            if(!quiet)
                warning("Data contains NA's.")
        }
    }

    # Third, we'll check to see if we have any character data
    if (!is.numeric(x)){
        if(!quiet)
            warning("The data does not appear to be numeric.")
        # Try to coerce the data
        # x = as.numeric(x)
    }

#     # Fourth, we'll see if we have more than one data point.
#     if (NROW(x) <= 1) {
#         if(!quiet)
#             warning("Only one row provided.  Returning NA.")
#         return(NA)
#     }

    # @todo: Add check for stopifnot(is.atomic(y))???

    return(as.vector(x))

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: checkDataVector.R,v 1.5 2007-02-28 02:47:34 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/02/28 02:09:03  peter
# - added checkDataMatrix() at the beginning to ensure that whatever object is entered would
# be coerced into a matrix before the vector checking
#
# Revision 1.3  2007/02/26 22:04:36  brian
# - changes in functions to pass "R CMD check" for package
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################