#' Blanco-Ihle forecast evaluation backtest measure
#'
#' Derives the Blanco-Ihle forecast evaluation loss measure for a VaR
#' risk measurement model.
#' 
#' @param Ra Vector of a portfolio profit and loss
#' @param Rb Vector of corresponding VaR forecasts
#' @param Rc Vector of corresponding Expected Tailed Loss forecasts
#' @param cl VaR confidence interval
#' @return Something
#' 
#' @references Dowd, Kevin. Measuring Market Risk, Wiley, 2007.
#' 
#' Blanco, C. and Ihle, G. How Good is Your Var? Using Backtesting to Assess
#' System Performance. Financial Engineering News, 1999.
#' 
#' @author Dinesh Acharya
#' @examples
#' 			# To be added
#'
#' @export
BlancoIhleBacktest <- function(Ra, Rb, Rc, cl){
  
  profit.loss <- as.vector(Ra)
  VaR <- as.vector(Rb)
  ETL <- as.vector(Rc)
  
  n <- length(profit.loss)
  p <- 1-cl
  excess.loss <- -profit.loss(-profit.loss>VaR) # Derives excess loss
  benchmark <- double(length(excess_loss))
  
  for (i in 1:length(excess_loss)){
	benchmark[i] <- (ETL[i]-VaR[i])/Var[i]
	score[i] <- (excess.loss[i]-VaR[i])/VaR[i]-benchmark[i]
  }
  
  # First Blanco-Ihle score measure
  return((2/n)*sum(score)^2)
}