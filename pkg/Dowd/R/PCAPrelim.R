#' PCAPrelim
#' 
#' Estimates VaR plot using principal components analysis
#' 
#' @param Ra Matrix return data set where each row is interpreted as a set of 
#' daily observations, and each column as the returns to each position in a 
#' portfolio
#' @param position.data Position-size vector, giving amount invested in each 
#' position
#' @references Dowd, K. Measuring Market Risk, Wiley, 2007.
#'
#' @author Dinesh Acharya
#' @examples
#' 
#'    # Computes PCA Prelim
#'    Ra <- matrix(rnorm(4*4), 4, 4)
#'    PCAPrelim(Ra)
#'
#' @import expm MASS
#'
#' @export
PCAPrelim <- function(Ra){
  rho <- as.matrix(Ra)
  corr.matrix <- rbind(cbind(rho%^%0, rho%^%1, rho%^%2, rho%^%3, rho%^%4, 
                             rho%^%5, rho%^%6, rho%^%7, rho%^%8, rho%^%9),
                       cbind(rho%^%1, rho%^%0, rho%^%1, rho%^%2, rho%^%3,
                             rho%^%4, rho%^%5, rho%^%6, rho%^%7, rho%^%8),
                       cbind(rho%^%2, rho%^%1, rho%^%0, rho%^%1, rho%^%2,
                             rho%^%3, rho%^%4, rho%^%5, rho%^%6, rho%^%7),
                       cbind(rho%^%3, rho%^%2, rho%^%1, rho%^%0, rho%^%1,
                             rho%^%2, rho%^%3, rho%^%4, rho%^%5, rho%^%6),
                       cbind(rho%^%4, rho%^%3, rho%^%2, rho%^%1, rho%^%0,
                             rho%^%1, rho%^%2, rho%^%3, rho%^%4, rho%^%5),
                       cbind(rho%^%5, rho%^%4, rho%^%3, rho%^%2, rho%^%1,
                             rho%^%0, rho%^%1, rho%^%2, rho%^%3, rho%^%4),
                       cbind(rho%^%6, rho%^%5, rho%^%4, rho%^%3, rho%^%2,
                             rho%^%1, rho%^%0, rho%^%1, rho%^%2, rho%^%3),
                       cbind(rho%^%7, rho%^%6, rho%^%5, rho%^%4, rho%^%3,
                             rho%^%2, rho%^%1, rho%^%0, rho%^%1, rho%^%2),
                       cbind(rho%^%8, rho%^%7, rho%^%6, rho%^%5, rho%^%4,
                             rho%^%3, rho%^%2, rho%^%1, rho%^%0, rho%^%1),
                       cbind(rho%^%9, rho%^%8, rho%^%7, rho%^%6, rho%^%5,
                             rho%^%4, rho%^%3, rho%^%2, rho%^%1, rho%^%0))
  mu <- double(10)
  sigma <- corr.matrix
  # Random number generation
  returns <- mvrnorm(1000, mu, sigma)
  # Dowd code uses princomp in matlab. Similar function "princomp" is available 
  # in "stats" package. However, the return values from princomp are not used
  # explicitly. So, following alternative was used.
  variances <- eigen(cov(returns))$values # eigenvalues of covariance matrix.
  
  # Scree Plot
  n <- 1000
  par(c(2,1))
  percent.explained <- 100 * variances / sum(variances)
  barplot(percent.explained, xlab = "%")
  
  cum.variance <- double(length(variances))
  cum.variance[1] <- percent.explained[1]
  for (i in 2:length(variances)) {
    cum.variances[i] <- percent.explained[i] + cum.variance[i-1]
  }
  t <- 0:10
  plot(t, c(0, cum_variance), xlab = "Principal component", ylab = "%")
  title("Explanatory Power of the Principal Components")
  
}
