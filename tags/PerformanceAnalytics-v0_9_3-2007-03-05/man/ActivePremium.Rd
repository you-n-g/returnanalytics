\name{ActivePremium}
\alias{ActivePremium}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Active Premium }
\description{
The return on an investment's annualized return minus the benchmark's annualized return.

Active Premium = Investment's annualized return - Benchmark's annualized return
}
\usage{
ActivePremium(R, Rb, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ return vector of the portfolio }
  \item{Rb}{ return vector of the benchmark asset }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\value{
ActivePremium (numeric)
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\note{}
\seealso{
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
}
\examples{
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
sp500 = download.SP500PriceReturns(start="1996-12-31",end=end)
ActivePremium(edhec[,"Long.Short.Equity"],sp500)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }