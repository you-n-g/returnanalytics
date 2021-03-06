\name{chart.TimeSeries}
\alias{chart.TimeSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creates a time series chart with some extensions. }
\description{
Draws a line chart and labels the x-axis with the appropriate dates.  This is really a "primitive", since it extends the base \code{\link{plot}} and standardizes the elements of a chart.  Adds attributes for shading areas of the timeline or aligning vertical lines along the timeline. This function is intended to be used inside other charting functions.
}
\usage{
chart.TimeSeries(R, auto.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, type = "l", lty = 1, lwd = 2, main = NULL, ylab = NULL, xlab = "Date", date.format.in = "\%Y-\%m-\%d", date.format = NULL, xlim = NULL, ylim = NULL, element.color = "darkgray", event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "aliceblue", colorset = (1:12), pch = (1:12), legend.loc = NULL, ylog = FALSE, cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, major.ticks = "auto", minor.ticks = TRUE, grid.color = "lightgray", grid.lty = "dotted", xaxis.labels = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{auto.grid}{ if true, draws a grid aligned with the points on the x and y axes }
  \item{grid.color}{ sets the color for the reference grid }
  \item{grid.lty}{ defines the line type for the grid }
  \item{xaxis}{ if true, draws the x axis }
  \item{yaxis}{ if true, draws the y axis }
  \item{yaxis.right}{ if true, draws the y axis on the right-hand side of the plot }
  \item{type}{ set the chart type, same as in \code{\link{plot}} }
  \item{lty}{ set the line type, same as in \code{\link{plot}} }
  \item{lwd}{ set the line width, same as in \code{\link{plot}} }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{date.format}{ re-format the dates for the xaxis; the default is "\%m/\%y" }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{event.lines}{ If not null, vertical lines will be drawn to indicate that an event happened during that time period.  \code{event.lines} should be a list of dates (e.g., \code{c("09/03","05/06"))} formatted the same as date.format.  This function matches the re-formatted row names (dates) with the events.list, so to get a match the formatting needs to be correct. }
  \item{event.labels}{ if not null and event.lines is not null, this will apply a list of text labels (e.g., \code{c("This Event", "That Event")} to the vertical lines drawn.  See the example below. }
  \item{period.areas}{ these are shaded areas described by start and end dates in the same format as the date.format.  This is provided as a list of pairs, e.g., \code{list(c("10/26","11/27"), c("08/29","03/33"))} See the examples below. }
  \item{event.color}{ draws the event described in \code{event.labels} in the color specified }
  \item{period.color}{ draws the shaded region described by \code{period.areas} in the color specified }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{pch}{ symbols to use, see also \code{\link{plot}} }
  \item{element.color}{ provides the color for drawing chart elements, such as the box lines, axis lines, etc. Default is "darkgray" } 
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{date.format.in}{ allows specification of other date formats in the data object, defaults to "\%Y-\%m-\%d" }
  \item{cex.axis}{ The magnification to be used for axis annotation relative to the current setting of 'cex', same as in \code{\link{plot}}.  }
  \item{cex.legend}{ The magnification to be used for sizing the legend relative to the current setting of 'cex'.  }
  \item{cex.labels}{ The magnification to be used for event line labels relative to the current setting of 'cex'.  }
  \item{cex.lab}{ The magnification to be used for x- and y-axis labels relative to the current setting of 'cex'.  }
  \item{cex.main}{ The magnification to be used for the chart title relative to the current setting of 'cex'.  }
  \item{major.ticks}{ Should major tickmarks be drawn and labeled, default 'auto'}
  \item{minor.ticks}{ Should minor tickmarks be drawn, default TRUE}
  \item{xaxis.labels}{ Allows for non-date labeling of date axes, default is NULL }
  \item{\dots}{ any other passthru parameters }

}
\author{ Peter Carl }
\seealso{ \code{\link{plot}}, \code{\link{par}}, \code{\link[xts]{axTicksByTime}} }
\examples{

# These are start and end dates, formatted the same way as the default axis labels
cycles.dates = list(
    c("Oct 26","Nov 27"),
    c("Aug 29","Mar 33"),
    c("May 37","Jun 38"),
    c("Feb 45","Oct 45"),
    c("Nov 48","Oct 49"),
    c("Jul 53","May 54"),
    c("Aug 57","Apr 58"),
    c("Apr 60","Feb 61"),
    c("Dec 69","Nov 70"),
    c("Nov 73","Mar 75"),
    c("Jan 80","Jul 80"),
    c("Jul 81","Nov 82"),
    c("Jul 90","Mar 91"),
    c("Mar 01","Nov 01"))
# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
risk.dates = c(
    "Oct 87",
    "Feb 94",
    "Jul 97",
    "Aug 98",
    "Oct 98",
    "Jul 00",
    "Sep 01")
risk.labels = c(
    "Black Monday",
    "Bond Crash",
    "Asian Crisis",
    "Russian Crisis",
    "LTCM",
    "Tech Bubble",
    "Sept 11")
data(edhec)

R=edhec[,"Funds of Funds",drop=FALSE]
Return.cumulative = cumprod(1+R) - 1
chart.TimeSeries(Return.cumulative)
chart.TimeSeries(Return.cumulative, colorset = "darkblue", legend.loc = "bottomright", period.areas = cycles.dates, period.color = "lightblue", event.lines = risk.dates, event.labels = risk.labels, event.color = "red", lwd = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
