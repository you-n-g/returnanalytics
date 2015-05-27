#' Christoffersen Backtest for Unconditional Coverage
#'
#' Carries out the Christiffersen backtest for unconditional coverage for a 
#' VaR risk measurement model, for specified VaR confidence level.
#' 
#' @param Ra Vector of portfolio profit and loss observations
#' @param Rb Vector of VaR forecasts corresponding to PandL observations
#' @param cl Confidence level for VaR
#' @return Probability, given the data set, that the null hypothesis (i.e.
#' a correct model) is correct.
#' 
#' @references Dowd, K. Measuring Market Risk, Wiley, 2007.
#' 
#' Christoffersen, P. Evaluating interval forecasts. International Economic 
#' Review, 39(4), 1998, 841-862.
#' 
#' @author Dinesh Acharya
#' @examples
#' 			# To be added
#'
#' @export
ChristoffersenBacktestForUnconditionalCoverage <- function(Ra, Rb, cl){
  
  profit.loss <- as.vector(Ra)
  VaR <- as.vector(Rb)
  
  n <- length(profit.loss) # Number of observations
  p <- 1-cl # Probability of failure under null hypothesis
  v <- length(which(VaR+profit.loss<0)) # Frequency of failures
  
  # Likelihood ratio test statistic
  LR <- -2*log(((p^x)*(1-p)^(n-x))/((phat^x)*((1-phat)^(n-x))))
  
  # Probability that null hypothesis (model) is correct
  return(1-pchisq(LR,1))
  
}