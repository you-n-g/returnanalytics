\name{BackTestTimes}
\alias{BackTestTimes}
\title{BackTesting Time Period Function}
\usage{
  BackTestTimes(dates, rebalance.periods, training.length)
}
\arguments{
  \item{dates}{A date object, such as calling index() on an
  xts object}
}
\value{
  returns a list training start and end dates, and
  rebalancing dates
}
\description{
  Calculates rebalance dates
}
\author{
  Doug Martin
}

