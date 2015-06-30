#' Calculates ES using Epanechinikov kernel approach
#' 
#' The output consists of a scalar ES for specified confidence level.
#' 
#' @param Ra Profit and Loss data set
#' @param cl ES confidence level
#' @return Scalar ES
#' @references Dowd, K. Measuring Market Risk, Wiley, 2007.
#'
#' @author Dinesh Acharya
#' @examples
#' 
#'    # ES for specified confidence level using Epanechinikov kernel approach
#'    Ra <- rnorm(30)
#'    KernelESEpanechinikovKernel(Ra, .95)
#'
#' @export
KernelESEpanechinikovKernel <- function(Ra, cl){
  PandL <- as.vector(Ra)
  n <- 1000
  delta.cl <- (1 - cl) / n
  VaR <- double(999)
  for (i in 1:(n - 1)) {
    VaR[i] <- KernelVaREpanechinikovKernel(PandL, cl + i * delta.cl)
  }
  ES <- mean(VaR)
  return(ES)
  
}