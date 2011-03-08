`page.CorHeatmap` = function(R, margin = c(10,10), lmat = rbind(c(0, 3), c(2,1), c(4,4)), lhei=c(1.5, 4, 1.5), lwid = c(0.1,6), trace="none", dendrogram="column", scale="none", ...)
{

    stopifnot("package:gplots" %in% search() || require("gplots",quietly=TRUE))
    R = checkData(R)
    COR= cor(R, use="pairwise.complete.obs")

    # @Todo: incorporate into squares?
    #      ## Graphical P-values (aka "significance stars"):
    #         Signif <- symnum(COR$p.value, corr = FALSE, na = FALSE,
    #             cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***",
    #                 "**", "*", ".", " "))
    #         text(0.5, 0.5, txt, cex = cex * r)
    #         text(0.8, 0.8, Signif, cex = cex, col = 2)

    heatmap.2(COR, distfun = function(x) as.dist((1 - x)/2), scale = scale, margin = margin, lmat = lmat, lhei = lhei, lwid = lwid, trace = trace, dendrogram = dendrogram, symm=TRUE, ...)

    # COR= cor(cls.x['2006::2008',c(3,4,6,7,14,26,28,30:32,34:39,40,43,44,46)], use="pairwise.complete.obs")
    # postscript("disguised-heatmap-tr36m.eps", width=4, height=6, paper="special", horizontal=FALSE, onefile=FALSE);heatmap.2(COR, distfun = function(c) as.dist((1 - c)/2), scale="none", trace="none", dendrogram="column", margin=c(12,12), lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1, 4, 1.5 ), lwid=c(.1,6)); dev.off()
}

###############################################################################
# pages: Presentation of performance and risk for assets and portfolios
# in R (see http://r-project.org/) 
# Copyright (c) 2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: page.CorHeatmap.R,v 1.2 2009-08-07 18:30:16 peter Exp $
#
################################################################################
# $Log: page.CorHeatmap.R,v $
# Revision 1.2  2009-08-07 18:30:16  peter
# - changed margin
#
# Revision 1.1  2009-06-02 02:59:21  peter
# - initial commit of function
#
