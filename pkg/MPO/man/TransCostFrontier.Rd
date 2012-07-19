\name{TransCostFrontier}
\alias{TransCostFrontier}
\title{Transaction cost penalized portfolio efficient frontier}
\usage{
  TransCostFrontier(returns, npoints = 10, min.lambda,
    max.lambda, w.initial, c, long.only = FALSE)
}
\arguments{
  \item{returns}{an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns}

  \item{min.lambda}{minimum feasible risk aversion
  parameter to use in optimization}

  \item{max.lambda}{maximum feasible risk aversion
  parameter to use in optimization}

  \item{w.initial}{initial vector of portfolio weights.
  Length of the vector must be equal to ncol(returns)}

  \item{c}{transaction costs.  Must be a single value or a
  vector of length equal to ncol(returns)}

  \item{long.only}{optional long only constraint.  Defaults
  to FALSE}
}
\value{
  returns a matrix, with the first column of mean return
  second column of portfolio standard deviation, and
  subsequent columns of asset weights
}
\description{
  Calculates an efficient frontier of portfolios using
  transaction costs as a penalty.
}
\author{
  James Hobbs
}
\seealso{
  \code{\link{TransactionCostOpt}}

  data(Returns) efront <-
  TransCostFrontier(large.cap.returns,npoints=50,min.lambda=5,
  max.lambda=1000,w.initial=rep(1/100,100),c=0.0005)
  plot(x=efront[,"SD"],y=efront[,"MU"],type="l")
}

