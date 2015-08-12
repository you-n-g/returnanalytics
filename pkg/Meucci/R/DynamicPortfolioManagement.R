#' Solves the Bellman Equation for the case study 1. 
#'
#' @details In Case Study 1 there is only one risk driver (the 10 year rate) and
#' only one view. The view is that the expected value of the 10-year rate will
#' be the actual value minus 50 basis points at t^view = 1 year from the current
#' time.
#' The solution is analytical in the prior case.
#' The solution is found recursivelly in the posterior case starting from
#' t_view and going back to time 0 with a time step = tau.
#'
#' In case study 1: n_ = 1 N_meanViews = 1 
#'
#' @param eta       [scalar] overall weight of the market impact of transactions
#' @param gamma     [scalar] risk aversion parameter
#' @param lambda    [scalar] discounting parameter
#' @param tau       [scalar] trading interval
#' @param theta     [n_ x n_] transition matrix of the MVOU process
#' @param mu        [n_ x 1]  drift vector of the MVOU process
#' @param sig2      [n_ x n_] covariance parameters of the MVOU process
#' @param c2        [n_ x n_] matrix of the market impact
#' @param b_legacy  [n_ x 1]  legacy portfolio exposure at time 0
#' @param x         [t_ x n_] path of the risk drivers (with time step = tau)
#' @param t_view    [1 x N_MeanViews] times of the views
#' @param view      [1 x N_MeanViews] views on the risk drivers
#'
#' @return prior  [t_x n_ matrix] optimal prior exposure
#' @return post   [t_x n_ matrix] optimal posterior exposure
#'
#' @references
#' A. Meucci - "Dynamic Portfolio Management with Views at Multiple Horizons"
#' \url{http://symmys.com/node/831}. See Meucci script for "BellmanEq_CS1.m"
#' 
#' @author Xavier Valls \email{xavievallspla@@gmail.com}
#' @export

BellmanEq_CS1 <- function(eta, gamma, lambda, tau, theta, mu, sig2, c2,
                          b_legacy, x, t_view, view) {
  t_ <- nrow(x)
  n_ <- length(theta)

  ##############################################################################
  #compute the prior at time 0
  Prior0 <- MVOU_Prior(c(0, tau), x[1], theta, sig2, mu)
  # first period covariance matrix
  sig2_1 <- Prior0$cov[(n_ + 1):(2 * n_), (n_ + 1):(2 * n_)]

  ##############################################################################
  #Coefficients of the Bellman equation according to the prior

  alpha_prior <- Prior0$mean_cost[(n_ + 1):(2 * n_)]
  beta_prior <- Prior0$mean_lin[(n_ + 1):(2 * n_), 1:n_] - diag(1, n_)
  HATsig2 <- exp(lambda) * (eta * c2) ^ (-1 / 2) * gamma * sig2_1 * (eta * c2) ^
             (-1 / 2)
  HATpsi_bb <- (0.25 * ( HATsig2 + diag(1, n_) * (exp(lambda) - 1)) ^ 2 +
                HATsig2) ^ (1 / 2) - 0.5 * (HATsig2 + diag(1, n_) *
               (exp(lambda) - 1))
  psi_bb_prior <- (eta * c2) ^ (1 / 2) * HATpsi_bb * (eta * c2) ^ (1 / 2)
  q_prior <- gamma * sig2_1 + eta * c2 + exp(-lambda) * psi_bb_prior
  tmp <- (eta * c2 * (solve(q_prior) %*% beta_prior))
  psi_bx_prior <- solve(diag(1, n_ ^ 2) - exp(-lambda) * kron(t(beta_prior) +
                   diag(1, n_), eta * ( c2 / q_prior))) %*% array(tmp)
  psi_bx_prior <- matrix(psi_bx_prior, nrow = n_)
  psi_b_prior <- solve(q_prior / (eta * c2) - exp(-lambda) * diag(1, n_)) %*%
                 (diag(1, n_) + exp(-lambda) * psi_bx_prior) * alpha_prior

  # #Alternatively, if and only if c2 <- sig2_1
  # a_ <- (sqrt(4*gamma*eta*exp(-lambda)+(gamma+(1-exp(-lambda))*eta)^2) -
  #        ((1-exp(-lambda))*eta+gamma))/(2*exp(-lambda))
  # psi_bb_prior <- a_*sig2_1
  # psi_bx_prior <- eta*(beta_prior)*inv((gamma+eta+exp(-lambda)*a_)*
  #                 diag(1, n_)-eta*exp(-lambda)*(beta_prior+diag(1, n_)))
  # psi_b_prior <- eta*(alpha_prior + exp(-lambda)*psi_bx_prior*alpha_prior)/
  #                (gamma+eta+exp(-lambda)*a_-exp(-lambda)*eta)

  ##############################################################################
  #Coefficients of the Bellman equation according to the posterior distribution
  ##############################################################################

  ##############################################################################
  #Inizialize the variables
  Hor <- ceil(max(t_view) / tau)
  psi_t_bb <- array(0, dim = c(n_,n_, Hor))
  q_t <- array(0, dim = c(n_,n_, Hor))
  psi_t_bx <- array(0, dim = c(n_,n_, Hor))
  psi_t_b <- matrix(0, n_, Hor)
  alpha_t <- matrix(0, n_, Hor)
  beta_t <- array(0, dim = c(n_,n_, Hor))

  ##############################################################################
  #Set the boundary conditions asintotically. After the last view, the
  #solution is equal to the prior
  psi_t_bb[1:n_, 1:n_, Hor] <- psi_bb_prior
  q_t[1:n_, 1:n_, Hor] <- q_prior
  psi_t_bx[1:n_, 1:n_, Hor] <- psi_bx_prior
  psi_t_b[1:n_, Hor] <- psi_b_prior
  alpha_t[1:n_, Hor] <- alpha_prior
  beta_t[1:n_, 1:n_, Hor] <- beta_prior

  for (k in seq(Hor - 1, 1, -1)) {
    q <- drop(q_t[1:n_, 1:n_, k + 1])
    alpha <- drop(alpha_t[1:n_, k + 1])
    beta <- drop(beta_t[1:n_, 1:n_, k + 1])
    psi_bx <- drop(psi_t_bx[1:n_, 1:n_, k + 1])
    psi_b <- drop(psi_t_b[1:n_, k + 1])

    ############################################################################
    #update of the coefficients of the Bellman equation
    psi_t_b[1:n_, k] <- eta * c2 * (solve(q) %*% (alpha + exp(-lambda) *
                        psi_bx * alpha + exp(-lambda) * psi_b))
    psi_t_bx[1:n_, 1:n_, k] <- eta * c2 * (solve(q) %*% (beta + exp(-lambda) *
                               psi_bx * (beta + diag(1, n_))))
    psi_t_bb[1:n_, 1:n_, k] <- -eta ^ 2 * c2 * (solve(q) %*% c2) + eta * c2

    ############################################################################
    #set the monitoring times of interest
    t <- c(0, tau, t_view - k * tau)
    if ((t[length(t)] - t[length(t) - 1]) < t[length(t)] * 10 ^ (-10))
      t <- t[-length(t)]
    T_ <- length(t)

    #compute the prior
    Prior <- MVOU_Prior(t, 0, theta, sig2, mu)

    #set the views
    grid <- meshgrid(t, 1:n_)
    T <- grid$X
    N <- grid$Y
    N_Meanviews <- 1  #Number of views on expectations
    v_mu_tmp <- array(0, dim = c(N_Meanviews, n_, T_))
    v_mu_tmp[1, 1, T_] <- 1
    mu_view <- view[1]
    v_mu <- matrix(v_mu_tmp, nrow = nrow(v_mu_tmp))
    views <- list()
    views$N_Meanviews <-  N_Meanviews
    views$N_Covviews <- c()
    views$dimension <- array(N)
    views$monitoring_time <- array(T)
    views$v_mu <- v_mu
    views$v_sig <- NaN
    views$mu_view <- mu_view
    views$sig2_view <- c()

    ############################################################################
    #update the Posterior moments of the process
    Posterior <- MVOU_Posterior(Prior, views)
    alpha_t[1:n_, k] <- Posterior$mean_cost[(n_ + 1):(2 * n_)]
    beta_t[1:n_, 1:n_, k] <- Posterior$mean_lin[(n_ + 1):(2 * n_), 1:n_] -
                                       diag(1, n_)
    sig2_1 <- Posterior$cov[(n_ + 1):(2 * n_), (n_ + 1):(2 * n_)]

    ############################################################################
    #update matrix q_t
    psi_bb <- drop(psi_t_bb[1:n_, 1:n_, k])
    q_t[1:n_, 1:n_, k] <- gamma * sig2_1 + eta * c2 + exp(-lambda) * psi_bb
  }

  b_prior <- matrix(NaN, t_, 1 )
  b_post <- matrix(NaN, t_, 1 )

  #Reconstructing the optimal exposure on the simulated path x
  for (t in 1:t_) {
    if (t == 1) {
      b_legacy_prior <- b_legacy
      b_legacy_post <- b_legacy
    } else {
      b_legacy_prior <- b_prior[t - 1]
      b_legacy_post <- b_post[t - 1]
    }
    # prior exposure
    b_prior[t] <- solve(q_prior) %*% (alpha_prior + (beta_prior) * x[t] + eta *
        c2 * b_legacy_prior + exp(-lambda) * psi_bx_prior * (alpha_prior +
        (beta_prior + diag(1, n_)) * x[t]) + exp(-lambda) * psi_b_prior)
    #posterior exposure
    q <- drop(q_t[1:n_, 1:n_, t])
    alpha <- drop(alpha_t[1:n_, t])
    beta <- drop(beta_t[1:n_, 1:n_, t])
    psi_bx <- drop(psi_t_bx[1:n_, 1:n_, t])
    psi_b <- drop(psi_t_b[1:n_, t])
    l <- alpha + beta * x[t] + eta * c2 * b_legacy_post + exp(-lambda) *
         psi_bx * (alpha + (beta + diag(1, n_)) * x[t]) + exp(-lambda) * psi_b
    b_post[t] <- solve(q) %*% l
  }
  return(list(prior = b_prior, post = b_post))
}

#' Solves the Bellman Equation for the case study 2. 
#'
#' @details In Case Study 2 we consider two risk drivers, the 10 year rate and
#' the TIP spread, and two non-synchronous views on them. The view on the rate
#' is that its expected value will be the actual value minus 50 basis points
#' at t_viewX = 1 year from the current time (as in Case Study 1).
#' The view on the TIP spread is that its expected value will be the actual
#' value plus 50 basis points at t_viewTIP = 0.75 years.
#'
#' In case study 2: n_ = 2 k_ = 1 N_meanViews = 2
#'
#' @param eta       [scalar] overall weight of the market impact of transactions
#' @param gamma     [scalar] risk aversion parameter
#' @param lambda    [scalar] discounting parameter
#' @param tau       [scalar] trading interval
#' @param theta     [n_ x n_] transition matrix of the MVOU process
#' @param mu        [n_ x 1]  drift vector of the MVOU process
#' @param sig2      [n_ x n_] covariance parameters of the MVOU process
#' @param c2        [n_ x n_] matrix of the market impact
#' @param b_legacy  [n_ x 1]  legacy portfolio exposure at time 0
#' @param x         [t_ x n_] path of the risk drivers (with time step = tau)
#' @param t_view    [1 x N_MeanViews] times of the views
#' @param view      [1 x N_MeanViews] views on the risk drivers
#' @param i_view    [1 x N_MeanViews] vector of the labels of the risk drivers
#'                                    to which views refer
#' @param omega     [k_ x n_] matrix to select the investible risk drivers
#'
#' @return prior  [t_x n_ matrix] optimal prior exposure
#' @return post   [t_x n_ matrix] optimal posterior exposure
#'
#' @references
#' A. Meucci - "Dynamic Portfolio Management with Views at Multiple Horizons"
#' \url{http://symmys.com/node/831}. See Meucci script for "BellmanEq_CS2.m"
#' 
#' @author Xavier Valls \email{xavievallspla@@gmail.com}
#' @export

BellmanEq_CS2 <- function(eta, gamma, lambda, tau, theta, mu, sig2, c2,
                          b_legacy, x, t_view, view, i_view, omega) {

  t_ <- nrow(x)
  n_ <- nrow(theta)
  k_ <- length(c2)

  ##############################################################################
  #compute the prior at time 0
  Prior0 <- MVOU_Prior(c(0, tau), matrix(x[1:n_]), theta, sig2, mu)
  # first period covariance matrix
  sig2_1 <- Prior0$cov[(n_ + 1):(2 * n_), (n_ + 1):(2 * n_)]
  print(omega)
  print(sig2_1)
  print(t(omega))
  sig2_1 <- omega %*% sig2_1 %*% t(omega)
  ##############################################################################
  #Coefficients of the Bellman equation according to the prior

  alpha_prior <- Prior0$mean_cost[(n_ + 1):(2 * n_)]
  beta_prior <- Prior0$mean_lin[(n_ + 1):(2 * n_), 1:n_] - diag(1, n_)
  HATsig2 <- exp(lambda) * (eta * c2) ^ (-1 / 2) * gamma * sig2_1 * (eta * c2) ^
             (-1 / 2)
  HATpsi_bb <- (0.25 * ( HATsig2 + diag(1, k_) * (exp(lambda) - 1)) ^ 2 +
                HATsig2) ^ (1 / 2) - 0.5 * (HATsig2 + diag(1, k_) *
               (exp(lambda) - 1))
  psi_bb_prior <- (eta * c2) ^ (1 / 2) * HATpsi_bb * (eta * c2) ^ (1 / 2)
  q_prior <- gamma * sig2_1 + eta * c2 + exp(-lambda) * psi_bb_prior
  tmp <- (eta * c2 * (solve(q_prior) %*% (omega %*% beta_prior)))
  psi_bx_prior <- solve(diag(1, k_ * n_) - exp(-lambda) * kron(t(beta_prior) +
                   diag(1, n_), eta * ( c2 / q_prior))) %*% array(tmp)
  psi_bx_prior <- matrix(psi_bx_prior, nrow = k_, ncol = n_)
  psi_b_prior <- solve(q_prior / (eta * c2) - exp(-lambda) * diag(1, k_)) %*%
                 (omega + exp(-lambda) * psi_bx_prior) %*% alpha_prior

  ##############################################################################
  #Coefficients of the Bellman equation according to the posterior distribution
  ##############################################################################

  ##############################################################################
  #Inizialize the variables
  Hor <- ceil(max(t_view) / tau)
  psi_t_bb <- array(0, dim = c( k_, k_, Hor))
  q_t <- array(0, dim = c( k_, k_, Hor))
  psi_t_bx <- array(0, dim = c( k_, n_, Hor))
  psi_t_b <- matrix(0, k_, Hor)
  alpha_t <- matrix(0, n_, Hor)
  beta_t <- array(0, dim = c( n_, n_, Hor))

  ##############################################################################
  #Set the boundary conditions asintotically. After the last view, the
  #solution is equal to the prior
  psi_t_bb[1:k_, 1:k_, Hor] <- psi_bb_prior
  q_t[1:k_, 1:k_, Hor] <- q_prior
  psi_t_bx[1:k_, 1:n_, Hor] <- psi_bx_prior
  psi_t_b[1:k_, Hor] <- psi_b_prior
  alpha_t[1:n_, Hor] <- alpha_prior
  beta_t[1:n_, 1:n_, Hor] <- beta_prior

  for (k in seq(Hor - 1, 1, -1)) {
    q <- drop(q_t[1:k_, 1:k_, k + 1])
    alpha <- drop(alpha_t[1:n_, k + 1])
    beta <- drop(beta_t[1:n_, 1:n_, k + 1])
    psi_bx <- drop(psi_t_bx[1:k_, 1:n_, k + 1])
    psi_b <- drop(psi_t_b[1:k_, k + 1])

    ############################################################################
    #update of the coefficients of the Bellman equation
    psi_t_b[1:k_, k] <- eta * c2 * (solve(q) %*% (omega %*% alpha +
                        exp(-lambda) * psi_bx %*% alpha + exp(-lambda) * psi_b))
    psi_t_bx[1:k_, 1:n_, k] <- eta * c2 * (solve(q) %*% (omega %*% beta +
                               exp(-lambda) * psi_bx %*% (beta + diag(1, n_))))
    psi_t_bb[1:k_, 1:k_, k] <- -eta ^ 2 * c2 * (solve(q) %*% c2) + eta * c2

    ############################################################################
    #set the monitoring times of interest
    t <- c(0, tau, t_view - k * tau)
    t <- sort(t)
    t <- t[t >= 0]
    idx <- which(diff(t) < tau * 10 ^ -10)
    t <- t(setdiff(1:length(t), idx))
    T_ <- length(t)

    #compute the prior
    Prior <- MVOU_Prior(t, matrix(c(0,0)), theta, sig2, mu)

    #set the views
    grid <- meshgrid(t, 1:n_)
    T <- grid$X
    N <- grid$Y
    views <- list()

    if ((k * tau) >= t_view[2]) {
      N_Meanviews <- 1  #Number of views on expectations
      v_mu_tmp <- array(0, dim = c(N_Meanviews, n_, T_))
      v_mu_tmp[1, i_view[1], T_] <- 1
      mu_view <- view[1]
    } else{
      N_Meanviews <- 2  #Number of views on expectations
      v_mu_tmp <- array(0, dim = c(N_Meanviews, n_, T_))
      v_mu_tmp[1, i_view[1], T_] <- 1
      v_mu_tmp[2, i_view[2], T_ - 1] <- 1
      mu_view <- view[1]
      mu_view <- c( mu_view, view[2])
    }

    v_mu <- matrix(v_mu_tmp, nrow = nrow(v_mu_tmp))
    views$N_Meanviews <-  N_Meanviews
    views$N_Covviews <- c()
    views$dimension <- array(N)
    views$monitoring_time <- array(T)
    views$v_mu <- v_mu
    views$v_sig <- NaN
    views$mu_view <- mu_view
    views$sig2_view <- c()

    ############################################################################
    #update the Posterior moments of the process
    Posterior <- MVOU_Posterior(Prior, views)
    alpha_t[1:n_, k] <- Posterior$mean_cost[(n_ + 1):(2 * n_)]
    beta_t[1:n_, 1:n_, k] <- Posterior$mean_lin[(n_ + 1):(2 * n_), 1:n_] -
                             diag(1, n_)
    sig2_1 <- Posterior$cov[(n_ + 1):(2 * n_), (n_ + 1):(2 * n_)]
    sig2_1 <- omega %*% sig2_1 %*% t(omega)

    ############################################################################
    #update matrix q_t
    psi_bb <- drop(psi_t_bb[1:k_, 1:k_, k])
    q_t[1:k_, 1:k_, k] <- gamma * sig2_1 + eta * c2 + exp(-lambda) * psi_bb
  }

  b_prior <- matrix(NaN, t_, 1 )
  b_post <- matrix(NaN, t_, 1 )

  #Reconstructing the optimal exposure on the simulated path x
  for (t in 1:t_) {
    if (t == 1) {
      b_legacy_prior <- b_legacy
      b_legacy_post <- b_legacy
    } else {
      b_legacy_prior <- b_prior[t - 1]
      b_legacy_post <- b_post[t - 1]
    }
    # prior exposure
    b_prior[t] <- solve(q_prior) %*% (omega %*% alpha_prior + omega %*%
                  beta_prior %*% x[t,] + eta * c2 %*% b_legacy_prior +
                  exp(-lambda) %*% psi_bx_prior %*% (alpha_prior + (beta_prior +
                  diag(1, n_)) %*% x[t,]) + exp(-lambda) * psi_b_prior)

    #posterior exposure
    q <- drop(q_t[1:k_, 1:k_, t])
    alpha <- drop(alpha_t[1:n_, t])
    beta <- drop(beta_t[1:n_, 1:n_, t])
    psi_bx <- drop(psi_t_bx[1:k_, 1:n_, t])
    psi_b <- drop(psi_t_b[1:k_, t])
    l <- omega %*% alpha + omega %*% beta %*% x[t,] + eta * c2 *
         b_legacy_post + exp(-lambda) %*% psi_bx %*% (alpha + (beta +
         diag(1, n_)) %*% x[t,]) + exp(-lambda) * psi_b
    b_post[t] <- solve(q) %*% l
  }
  return(list(prior = b_prior, post = b_post))
}
