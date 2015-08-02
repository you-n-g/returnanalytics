#' @title Computes the conditional mean and covariance matrix at different
#' dates
#'
#' @description This function computes the conditional mean and covariance
#' matrix at different monitoring dates t1,t2,...,t_ of  the process
#' X_{t1,t2...,t_} =(X_t1, 
#'                   X_t2,
#'                   .
#'                   .
#'                   X_t_)
#' X_t follows a MVOU process: dX_t <- (-theta*X_t+mu)dt + sig*dB_t
#'
#' @param   t       [t_ x 1]  vector of monitoring dates
#' @param   x0      [n_ x 1]  observation at time 0
#' @param   theta   [n_ x n_] transition matrix
#' @param   sig2    [n_ x n_] covariance matrix
#' @param   mu      [n_ x 1]  vector of drift parameters
#'
#' @return  Mom     list of the risk drivers
#'
#' @references
#' A. Meucci - "Dynamic Portfolio Management with Views at Multiple Horizons"
#' \url{http://symmys.com/node/831}. See Meucci script for "MVOU_Prior.m"
#' 
#' @author Xavier Valls \email{xavievallspla@@gmail.com}
#'
#' @export

#OUTPUT
#Mom.monitoring_time <- monitoring times             [t_*n_ x 1]
#Mom.dimension <- labels of the risk drivers         [t_*n_ x 1]
#Mom.cov <- covariance matrix of X_{t1,t2...t_}      [t_*n_ x t_*n_]
#Mom.mean <- vector of the means of X_{t1,t2...t_}   [t_*n_ x 1]
#Mom.mean_cost                                       [t_*n_ x 1]
#Mom.mean_lin                                        [t_*n_ x 1]
#Mom.mean_cost and Mom.mean_lin are such that
#t_ <- length(x)       

MVOU_Prior <- function (t, x0, theta, sig2, mu) {

  Tol_eigb <- 10 ^ -8
  t_ <- length(t)
  n_ <- length(x0)
  grid <- meshgrid(t, 1:n_)
  Mom <- list()
  Mom$monitoring_time <- grid$X
  Mom$dimension <- t(grid$Y)
  Mom$mean <- array(NaN, dim <- c(t_ * n_, 1))
  Mom$cov <- array(NaN,  dim <- c(t_ * n_, n_ * t_))
  Mom$mean_cost <- array(NaN,  dim <- c(t_ * n_, 1))
  Mom$mean_lin <- array(NaN,  dim <- c(t_ * n_, n_))

  kronsum <- kronecker(theta, diag(1, n_)) + kronecker(diag(1, n_), theta)
  e <- eigen(kronsum)
  V <- e$vectors
  D <- e$values
  lambda_A <- array( NaN, length(D))

  M <- matrix(NaN, n_, t_)
  mean_lin <- array(NaN, dim <- c(n_, n_, t_))
  mean_cost <- matrix(NaN, n_, t_)
  e <- eigen(theta)
  V1 <- e$vectors
  theta_diag <- e$values

  F <- array(NaN, n_)

  for (i in 1:t_) {
    F[theta_diag <= Tol_eigb] <- t[i]
    F[theta_diag > Tol_eigb]  <- (1 - exp(-theta_diag[theta_diag > Tol_eigb] *
                                  t[i])) / theta_diag[theta_diag > Tol_eigb]
    E <- expm(-theta * t[i])
    M[ 1:n_, i] <- (E %*% x0 + V1 %*% diag(F) %*% pinv(V1) %*% mu)
    M[, i] <- Re(M[,i])

    mean_lin[1:n_, 1:n_, i] <- t(E)
    mean_lin[ , , i] <- Re(mean_lin[, , i] )
    mean_cost[1:n_, i] <- (V1 %*% diag(F) %*% pinv(V1) %*% mu)
    mean_cost[ , i] <- Re(mean_cost[, i])

    vecsig2 <- matrix(sig2, nrow = n_ ^ 2)
    lambda_A[(abs(lambda) <= Tol_eigb)] <- t[i]
    index <- abs(lambda) > Tol_eigb
    lambda_A[index] <- (1 - exp(-lambda[index] * t[i])) / lambda[index]
    A <- V %*% diag(lambda_A) %*% solve(V)
    vecsig2_t <- A %*% vecsig2
    sig2_t <- matrix(vecsig2_t, nrow = n_)
    sig2_t <- Re(sig2_t)

    for ( j in i:t_ ) {
      Mom$cov[(i - 1) * n_ + (1:n_), (j - 1) * n_ + (1:n_)] <- sig2_t %*%
                                                             expm(-t(theta) *
                                                                  (t[j] - t[i]))
      Mom$cov[(j - 1) * n_ + (1:n_), (i - 1) * n_ + (1:n_)] <- expm(-theta *
                                                              (t[j] - t[i])) %*%
                                                               sig2_t
    }
  }
  #Note that Mom.mean <- Mom.mean_cost + Mom.mean_lin*x0
  Mom$mean <- matrix(M, ncol = 1)
  Mom$mean_lin <- t(array(mean_lin, c(dim(mean_lin)[1], dim(mean_lin)[2] *
                                                        dim(mean_lin)[3])))
  Mom$mean_cost <- t(matrix(mean_cost, nrow = 1))

  return(Mom)
}
