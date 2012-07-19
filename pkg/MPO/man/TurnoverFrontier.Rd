\name{TurnoverFrontier}
\alias{TurnoverFrontier}
\title{Turnover constrained portfolio frontier}
\usage{
  TurnoverFrontier(returns, npoints = 10, minmu, maxmu,
    w.initial, turnover, long.only = FALSE)
}
\arguments{
  \item{returns}{an xts, vector, matrix, data frame,
  timeSeries or zoo object of asset returns}

  \item{minmu}{min feasible target portfolio return to use
  in optimization}

  \item{maxmu}{max feasible target portfolio return to use
  in optimization}

  \item{w.initial}{initial vector of portfolio weights.
  Length of the vector must be equal to ncol(returns)}

  \item{turnover}{constraint on turnover from intial
  weights}

  \item{long.only}{optional long only constraint.  Defaults
  to FALSE}
}
\value{
  returns a matrix, with the first column of mean return
  second column of portfolio standard deviation, and
  subsequent columns of asset weights
}
\description{
  Calculates an efficient frontier of portfolios with a
  constraint on overall turnover
}
\author{
  James Hobbs
}
\seealso{
  \code{\link{TurnoverOpt}}

  data(Returns) efront <-
  TurnoverFrontier(large.cap.returns,npoints=50,minmu=0.001,
  maxmu=.05, w.initial=rep(1/100,100),turnover=5)
  plot(x=efront[,"SD"],y=efront[,"MU"],type="l")
}

