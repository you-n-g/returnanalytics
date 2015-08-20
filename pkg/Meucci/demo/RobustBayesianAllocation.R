#' Example of Robust Bayesian allocation in the stock market. Case study of the
#' simulations, as described in A. Meucci, "Robust Bayesian Allocation". 
#'  
#' @references 
#' A. Meucci - "Robust Bayesian Allocation"
#' \url{http://symmys.com/node/102}
#'
#' See Meucci's script for "S_SimulationsCaseStudy.m" and MATLAB package
#' "Meucci_RobustBayesian" for original MATLAB
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}

library(MASS)
####################################################################
# inputs
####################################################################
J <- 50 # number of simulations
T <- 52 # number of observations in time series
N <- 20 # number of assets in the market
r <- .4 # overall correlation in constant correlation matrix
min_s <- .1 # min volatility among the N assets
max_s <- .4 # max volatility among the N assets
NumPortf <- 10 # number of discretizations for efficient frontier
p_m <- .1 # aversion to estimation risk for mu
p_s <- .1 # aversion to estimation risk for sigma

####################################################################
# true market parameters
####################################################################
# creates a homogenous correlation matrix
C <- (1 - r) * eye(N) + r * ones(N, N)
# 1st asset will have min volatility...
step_s <- (max_s - min_s) / (N - 1)
# ... last asset will have maximum volatility
s <- seq(min_s, max_s, step_s)
# fake covariance matrix with equally spaced volatilities
S <- diag(s) %*% C %*% diag(s)

# Note the means are defined in such a way that a mean-variance optimization
# would yield an equally weighted portfolio.
# fake mean matrix : mus = 2.5 * Sigma / N
M <- 2.5 * S %*% array(1, N) / N

####################################################################
# conduct Monte carlo simulation
####################################################################

# initialize variables
meanVarMus <- list()
meanVarVols <- list()
trueMus <- list()
trueVols <- list()
bayesianMus <- list()
bayesianVols <- list()
robustMus <- list()
robustVols <- list()

# construct efficient sample, bayesian, and robust bayesian frontier for each
# simulation
for (j in 1:J) {
  # Sample T draws from the true covariance matrix
  rets <- mvrnorm(T, M, S)

  # construct mean-variance frontier using sample estimate.
  mean <- colMeans(rets) # get mean vector
  cov <- cov(rets) # cov vector
  # returns a list of returns, volatility, and assets weights along the frontier
  # Each row represents a point on the frontier
  sampleFrontier <- efficientFrontier(NumPortf, cov, mean, TRUE)

  # construct mean-variance efficient portfolio based on true Mu and sigma
  trueFrontier <- efficientFrontier(NumPortf, S, M, TRUE)

  # Bayesian prior for covariance and mu's (an arbitrary prior model of
  # covariance and returns)
  # the covariance prior is equal to the sample covariance on the principal
  # diagonal
  cov_prior  <- diag(diag(cov))

  # set the prior expected returns for each asset to : mus = .5 * Sigma(1/N)
  # Incidentally, this ensures there is a perfect positive linear relationship
  # between asset variance and asset expected return
  mean_prior <- .5 * cov_prior %*% rep(1 / N, N)

  # set the confidence in the prior as twice the confidence in the sample and
  # blend the prior with the sample data
  posterior <- PartialConfidencePosterior(mean_sample = mean,
                                           cov_sample = cov,
                                           mean_prior = mean_prior,
                                           cov_prior = cov_prior,
                                           relativeConfidenceInMeanPrior = 2,
                                           relativeConfidenceInCovPrior = 2,
                                           sampleSize = nrow(rets))

  cov_post <- posterior$cov_post
  mean_post <- posterior$mean_post
  time_post <- posterior$time_post
  nu_post <- posterior$nu_post
  rm(posterior)

  # construct Bayesian frontier using blended mu and Sigma, and identify robust
  # portfolio.
  # returns a set of Bayesian efficient portfolios: a list of returns,
  # volatility, and assets weights along the posterior frontier.
  # Each row represents a point on the frontier and the returns, volatility,
  # and assets of the most robust portfolio in the set

  # Picks the 80% highest volatility (a parameter)...
  pickVol <- round(.8 * NumPortf)
  # ...on the sample *efficient* frontier. On the efficient *sample* frontier.
  # This is why the problem is a second-order cone programming problem.
  # TODO: why use sample frontier?
  volatility <- (sampleFrontier[["volatility"]][pickVol]) ^ 2

  if (is.na(volatility) == TRUE)
    stop("The chosen volatility is too high")

  frontierResults <- robustBayesianPortfolioOptimization(mean_post = mean_post,
                     cov_post = cov_post, nu_post = nu_post,
                     time_post = time_post, riskAversionMu = p_m,
                     riskAversionSigma = p_s, discretizations = NumPortf,
                     longonly = FALSE, volatility = volatility)

  bayesianFrontier <- frontierResults$bayesianFrontier
  robustPortfolio <- frontierResults$robustPortfolio
  rm(frontierResults)

  # initialize parameters
  mumv <- NULL
  volmv <- NULL
  mutrue <- NULL
  voltrue <- NULL
  mubay <- NULL
  volbay <- NULL

  # for each  portfolios along the sample and Bayesian frontiers, measure the
  # actual returns and actual volatility based on the true returns/true
  # covariance
  for (k in 1:(NumPortf - 1)) {
    # Notice when plotting the sample-based allocation is broadly scattered in
    # inefficient regions

    # retrieve the weights assigned to the k'th portfolio along the sample
    # frontier
    weightsMV <- sampleFrontier[["weights"]][k,]
    # given the optimal weights from sample estimates of mu and Sigma, measure
    # the actual return using the true asset means
    mumv <- c(mumv, weightsMV %*% M)
    # given the optimal weights from the sample estimates of mu and Sigma,
    # measure the actual variance of the portfolio
    volmv <- c(volmv, (weightsMV %*% S %*% weightsMV))

    # measure actual performance using true mu and cov

    # retrieve the weights assigned to the k'th portfolio along the true
    # frontier
    weightsMVTrue <- trueFrontier[["weights"]][k,]
    # given the optimal weights from actual values of mu and Sigma, measure the
    # actual return using the true asset means
    mutrue <- c(mutrue, weightsMVTrue %*% M)
    # given the optimal weights from the sample estimates of mu and Sigma,
    # measure the actual variance of the portfolio
    voltrue <- c(voltrue, (weightsMVTrue %*% S %*% weightsMVTrue))

    weightsBay <- bayesianFrontier[["weights"]][k,]
    # given the optimal weights from Bayesian estimates of mu and Sigma, measure
    # the actual return using the true asset means
    mubay <- c(mubay, weightsBay %*% M)
    # given the optimal weights from the Bayesian estimates of mu and Sigma,
    # measure the actual variance of the portfolio
    volbay <- c(volbay, (weightsBay %*% S %*% weightsBay))
   }

    # measure the actual performance of the most Robust portfolio along the
    # Bayesian efficient frontier
    weightsRob <- robustPortfolio$weights
    murob <- weightsRob %*% M
    volrob <- weightsRob %*% S %*% weightsRob

    # collect actual return and actual variances results for each portfolio in
    # each simulation

    # list of actual returns along efficient frontier for each simulation based
    # on portfolio constructed using sample mean and sample co-variance
    meanVarMus[[j]] <- mumv
    # ...and the list of actual variances along efficient frontier
    meanVarVols[[j]] <- volmv
    # list of actual returns based on bayesian mixing of prior and data sampled
    # from true distribution
    bayesianMus[[j]] <- mubay
    # ...and the list of associated actual variances
    bayesianVols[[j]] <- volbay
    # list of actual return based on robust allocation...
    # Note only one robust portfolio per simulation j is identified.
    robustMus[[j]] <- murob
    # ...and the list of associated actual variances.
    # Note only one robust portfolio per simulation j is identified.
    robustVols[[j]] <- volrob
    # list of actual return based on optimizing with the true mus...
    trueMus[[j]] <- mutrue
    # ...and the list of associated actual variances
    trueVols[[j]] <- voltrue
}

# Plot sample, bayesian, and robust mean/variance portfolios
library(ggplot2)

# create dataframe consisting of actual returns, actual variance, and sample
# indicator    
actualReturns <- unlist(meanVarMus)
actualVariance <- unlist(meanVarVols)
plotData1 <- data.frame(actualReturns = actualReturns,
                        actualVariance = actualVariance, type = "Sample")
actualReturns <- unlist(bayesianMus)
actualVariance <- unlist(bayesianVols)
plotData2 <- data.frame(actualReturns = actualReturns,
                        actualVariance = actualVariance, type = "Bayesian")
actualReturns <- unlist(robustMus)
actualVariance <- unlist(robustVols)
plotData3 <- data.frame(actualReturns = actualReturns,
                        actualVariance = actualVariance,
                        type = "Robust Bayesian")
actualReturns <- unlist(trueMus)
actualVariance <- unlist(trueVols)
plotData4 <- data.frame(actualReturns = actualReturns,
                        actualVariance = actualVariance, type = "True frontier")
plotData <- rbind(plotData1, plotData2, plotData3, plotData4)
rm(plotData1, plotData2, plotData3, actualReturns, actualVariance)

# build plot with overlays    
# Notice when plotting the the Bayesian portfolios are shrunk toward the prior.
# Therefore they are less scattered and more efficient, although the prior
# differs significantly from the true market parameters.
ggplot(data = plotData) + geom_point(aes_string(x = "actualVariance",
                                                y = "actualReturns",
                                                color = "type" ))
