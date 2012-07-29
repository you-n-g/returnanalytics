\name{ProportionalCostOpt}
\alias{ProportionalCostOpt}
\title{Proportional cost portfolio optimization}
\usage{
  ProportionalCostOpt(returns, mu.target, w.initial, tc,
    long.only = FALSE)
}
\arguments{
  \item{returns}{an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns}

  \item{mu.target}{target portfolio return}

  \item{w.initial}{initial vector of portfolio weights.
  Length of the vector must be equal to ncol(returns)}

  \item{tc}{proportional transaction cost}

  \item{long.only}{optional long only constraint.  Defaults
  to FALSE}
}
\value{
  returns a list with initial weights, buys, sells, and the
  aggregate of all three.  Also returns the portfolio's
  expected return and variance
}
\description{
  Calculate portfolio weights, variance, and mean return,
  given a set of returns and a value for proportional
  transaction costs
}
\author{
  James Hobbs
}
\seealso{
  \code{\link{TurnoverFrontier}}

  \code{\link{solve.QP}}

  data(Returns) opt <-
  ProportionalCostOpt(large.cap.returns,mu.target=0.004,
  w.initial = rep(1/100,100),tc=.01) opt$w.total
  opt$port.var opt$port.mu
}

