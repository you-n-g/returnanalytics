#' Generates arithmetric returns and arithmetric covariance matrix given a
#' distribution of log returns
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}


# Example experiment with two assets
library(MASS)

# initialize with average log returns and log-based covariance matrix
m1 <- c(.05, .12, .1)
S1 <- matrix(c(.1, .05, .02, .05, .1, .03, .02, .03, .1), nrow = 3)

# simulate log-return draws from log-based covariance matrix assuming normal
# distribution
set.seed(1001)
logReturns <- mvrnorm(2000000, mu = m1, Sigma = S1)

# convert to arithmetic returns
arithmeticReturn = exp(logReturns) - 1
colMeans(arithmeticReturn)

# create arithmetric based covariance matrix
var(arithmeticReturn)

# compare simulation results with linreturn function    
linreturn(m1, S1)
