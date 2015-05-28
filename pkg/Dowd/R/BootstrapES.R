#' Bootstrapped ES for specified confidence level
#'
#' Estimates the bootstrapped ES for confidence level and holding period
#' implied by data frequency.
#'
#' @param Ra Vector corresponding to profit and loss distribution
#' @param number.sample Number of samples to be taken in bootstrap procedure
#' @return cl Number corresponding to Value at Risk confidence level
#' 
#' @references Dowd, K. Measuring Market Risk, Wiley, 2007.
#' 
#' 
#' @author Dinesh Acharya
#' @examples
#' 
#'    # Estimates bootstrapped Es for given parameters
#'    a <- rnorm(100) # generate a random profit/loss vector
#'    BootstrappedES(a, 50, 0.95)
#'
#' @export
BootstrapES <- function(Ra, number.sample, cl){
  
  if (nargs() < 3){
    error("Too few arguments")
  }
  if (nargs() > 3){
    error("Too many arguments")
  }
  
  profit.loss.data <- as.vector(Ra)
  # Preprocess data
  unsorted.loss.data <- -profit.loss.data
  losses.data <- sort(unsorted.loss.data)
  n <- length(losses.data)
  
  # Check that inputs have correct dimensions
  if (length(cl) != 1) {
    error("Confidence level must be a scalar")
  }
  if (length(number.samples) != 1){
    error("Number of resamples must be a scalar");
  }
  
  # Check that inputs obey sign and value restrictions
  if (cl >= 1){
    stop("Confidence level must be less that 1")
  }
  if (number.resamples <= 0){
    stop("Number of resamples must be at least 0")
  }
  
  #############################################
  # suitable alternative to bootstrp in R is still to be explored.
  #############################################
  # ES estimation
  #
  # es <- bootstrp(number.resamples, "hses", losses.data, cl)
  # y <- mean(es)
  # return (y)
}