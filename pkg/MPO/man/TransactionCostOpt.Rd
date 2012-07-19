\name{TransactionCostOpt}
\alias{TransactionCostOpt}
\title{Quadratic Portfolio Optimization with transaction costs}
\usage{
  TransactionCostOpt(returns, lambda, w.initial, c,
    long.only = FALSE)
}
\arguments{
  \item{returns}{an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns}

  \item{lambda}{a risk aversion parameter}

  \item{w.initial}{initial vector of portfolio weights.
  Length of the vector must be equal to ncol(returns)}

  \item{c}{transaction costs.  Must be a single value or a
  vector of length equal to ncol(returns)}

  \item{long.only}{optional long only constraint.  Defaults
  to FALSE}
}
\value{
  returns a list with portfolio weights, return, and
  variance
}
\description{
  2 step utility maximization including tranasaction costs
  as a penalty
}
\author{
  James Hobbs
}
\seealso{
  \code{\link{TransCostFrontier}}

  data(Returns) opt <-
  TransactionCostOpt(large.cap.returns,w.initial=rep(1/100,100),
  lambda=1,c=.0005)
}

