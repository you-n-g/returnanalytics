#' @title Computes the matrix q_t of the problem to solve when using
#' CALCULUS of VARIATION
#'
#' @description This function computes the matrix q_t of the problem to solve
#' when using CALCULUS of VARIATION:
#' argmin_b (b' q_t b - b'l_t)
#' 
#' @param lambda   [scalar] discounting parameter
#' @param gamma    [scalar] risk aversion parameter
#' @param eta      [scalar] overall weight of the market impact of transactions
#' @param sig2     [n_*t_ x n_*t_] covariance matrix of the process of the risk
#'                                 drivers
#' @param c2       [k_ x k_] market impact matrix
#' @param tau_     [scalar] effective number of future time steps considered
#' @param n_       [scalar] number of risk drivers
#' @param i_invest [k_ x 1] labels of the investible risk drivers
#'
#' @return q_t   [k_*tau_ x k_*tau_] matrix computed
#' @note
#' t_ = number of monitoring times at which sig2 is computed
#' k_ = number of investible risk drivers
#'
#'
#' @references
#' A. Meucci - "Dynamic Portfolio Management with Views at Multiple Horizons"
#' \url{http://symmys.com/node/831}. See Meucci script for "MVOU_Prior.m"
#'
#' @author Xavier Valls \email{xavievallspla@@gmail.com}
#' @export

QuadraticMat_Vc <- function(lambda, gamma, eta, sig2, c2, tau_, n_,
                                                   i_invest = NULL) {

  if(length(i_invest) == 0)
    i_invest <- 1:length(c2)


  ExtractBlockMtx <- function( A, t1, t2, n1_, n2_, i_invest){
    # This function extracts the (t1,t2)-block out of the block-diagonal matrix
    # A and then it selects the indices given by i_invest
    # A is a matrix t_*n1_ x t_*n2_. The matrix has t_ blocks. Each block is
    # n1_ x n2_
    blk <- A[(1 + (t1 - 1) * n1_):(t1 * n1_), (1 + (t2 - 1) * n2_):(t2 * n2_)]
    if(length(blk) > 1)
        blk <- blk[i_invest, i_invest]
    return(blk)
  }

  k_ <- length(i_invest)  #number of investible risk drivers
  q_t <- matrix(0, k_ * tau_, k_ * tau_)
  for (t in 1:(tau_ - 1)) {
    sig2_t   <- ExtractBlockMtx(sig2, t, t, n_, n_, i_invest)
    sig2_t1  <- ExtractBlockMtx(sig2, t + 1, t + 1, n_, n_, i_invest)
    sig2_tt1 <- ExtractBlockMtx(sig2, t, t + 1, n_, n_, i_invest)
    sig2_t <- sig2_t + sig2_t1 - 2 * sig2_tt1
    q_t[((t - 1) * k_ + 1):(t * k_), ((t - 1) * k_ + 1):(t * k_)] <-
     exp(-lambda * (t - 1)) * (-gamma * 0.5 * sig2_t - eta * 0.5 * c2 *
     (1 + exp(-lambda)))
    q_t[((t - 1) * k_ + 1):(t * k_), (t * k_ + 1):((t + 1) * k_)] <-
     exp(-lambda * t) * eta * 0.5 * c2
    q_t[(t * k_ + 1):((t + 1) * k_), ((t - 1) * k_ + 1):(t * k_)] <-
     exp(-lambda * t) * eta * 0.5 * c2
  }

  t <- tau_
  sig2_t   <- ExtractBlockMtx(sig2, t, t, n_, n_, i_invest)
  sig2_t1  <- ExtractBlockMtx(sig2, t + 1, t + 1, n_, n_, i_invest)
  sig2_tt1 <- ExtractBlockMtx(sig2, t, t + 1, n_, n_, i_invest)
  sig2_t <- sig2_t + sig2_t1 - 2 * sig2_tt1
  q_t[((t - 1) * k_ + 1):(t * k_), ((t - 1) * k_ + 1):(t * k_)] <-
    exp(-lambda * (t - 1)) * (-gamma * 0.5 * sig2_t - eta * 0.5 * c2)
  q_t <- (q_t + t(q_t)) / 2

  return(q_t)
}
