#' BackTesting Time Period Function
#' 
#' Calculates rebalance dates
#' 
#' @param dates A date object, such as calling index() on an xts object
#' @rebalance.periods frequency of rebalancing periods
#' @training.length length of training data
#' @return returns a list training start and end dates, and rebalancing dates
#' @author Doug Martin
#' @export
BackTestTimes <- function(dates, rebalance.periods, training.length) {
    
    lds = length(dates)
    start.point = training.length
    lds1 = lds - 1
    reb.seq = seq(start.point, lds1, by = rebalance.periods)
    reb.dates <- dates[reb.seq]
    train.start.dates <- dates[reb.seq - training.length + 1]
    train.end.dates <- dates[reb.seq]
    list(train.start.dates = train.start.dates, 
        train.end.dates = train.end.dates, reb.dates = reb.dates)
}

#' BackTesting Portfolio Weights Function
#' 
#' Calculates rebalance dates
#' 
#' @param returns an xts object of asset returns
#' @rebalance.periods frequency of rebalancing periods
#' @training.length length of training data
#' @return returns an xts object of portfolio weights
#' @author Doug Martin
#' @authoer James Hobbs
#' @export
BackTestWeights = function(returns, rebalance.periods, training.length, FUN, ...)
{
  times <- BackTestTimes(index(returns),rebalance.periods,training.length)
  npts <- length(times$reb.dates)
  weight <- xts(matrix(NA, ncol = ncol(returns), nrow = npts), order.by = times$reb.dates)
  
  pb=txtProgressBar(min = 0, max = npts, style = 3)
  for (i in 1:npts) {
    # set key dates
    train.start.date = times$train.start.dates[i]
    train.end.date = times$train.end.dates[i]
    
    # fetch data
    ret.data = window(returns, start = train.start.date, end = train.end.date)
    
    FUN <- match.fun(FUN)
    port <- FUN(ret.data, ...=...)
    w <- port$w
    weight[i,] <- w
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(weight)
}



