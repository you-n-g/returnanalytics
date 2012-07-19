\name{TurnoverOpt}
\alias{TurnoverOpt}
\title{Turnover constrained portfolio optimization}
\usage{
  TurnoverOpt(returns, mu.target, w.initial, turnover,
    long.only = FALSE)
}
\arguments{
  \item{returns}{an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns}

  \item{mu.target}{target portfolio return}

  \item{w.initial}{initial vector of portfolio weights.
  Length of the vector must be equal to ncol(returns)}

  \item{turnover}{constraint on turnover from intial
  weights}

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
  given a set of returns and a constraint on overall
  turnover
}
\author{
  James Hobbs
}
\seealso{
  \code{\link{TurnoverFrontier}}

  \code{\link{solve.QP}}

  data(Returns) opt <-
  TurnoverOpt(large.cap.returns,mu.target=0.01, w.initial =
  rep(1/100,100),turnover=5) opt$w.total opt$port.var
  opt$port.mu
}

