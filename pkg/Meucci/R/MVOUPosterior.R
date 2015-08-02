#' @title Computes the posterior conditional expectation & covariance matrix of
#' 
#' @description This function computes the  posterior conditional expectation
#' and covariance matrix at different monitoring dates t1,t2,...,t_ of the process
#' X_{t1,t2...,t_} =(X_t1, 
#'                   X_t2,
#'                   .
#'                   .
#'                   X_t_)
#' X_t follows a MVOU process: dX_t <- (-theta*X_t+mu)dt + sig*dB_t
#' The Views are: E{v_mu*X}=mu_view and Cov{v_sig*X}=sig2_view
#'
#' @param   Mom       list  list of risk drivesrs
#' @param   Views     list  list of views
#'
#' @return  Posterior    list of posterior distribution information
#' 
#' @details The list Mom consists of.
#' \itemize{
#' \item{Mom$monitoring_time }{ monitoring times =t_*n_ x 1]}
#' \item{Mom$dimension }{ labels of the risk drivers [t_*n_ x 1]}
#' \item{Mom$cov }{ prior covariance matrix of X_\{t1,t2...t_\} [t_*n_ x t_*n_]}
#' \item{Mom$mean }{ prior vector of the means of X_\{t1,t2...t_\} [t_*n_ x 1]}
#' \item{Mom$mean_cost}{[t_*n_ x 1]}
#' \item{Mom$mean_lin}{[t_*n_ x 1]}
#' \item{}{Mom$mean_cost and Mom$mean_lin are such that Mom$mean_cost +
#' Mom$mean_lin*x0  = Mom$mean}}
#' 
#' while the list of Views has the elements:
#' \itemize{
#' \item{Views$N_MeanViews }{ Number of views on the expectations [scalar]}
#' \item{Views$N_CovViews }{ Number of views on the covariance matrix [scalar]}
#' \item{Views$dimension }{ labels of the risk drivers [t_*n_ x 1]}
#' \item{Views$monitoring_time }{ monitoring times [t_*n_ x 1]}
#' \item{Views$v_mu }{ matrix that qualifies the views on expectations 
#'              [N_MeanViews x t_*n_]}
#' \item{Views$v_sig }{ matrix that qualifies the views on covariance 
#'              [N_CovViews x t_*n_]}
#' \item{Views$mu_view }{ extent of the views on expectation [N_MeanViews x 1]}
#' \item{Views$sig2_view }{ extent of the views on the covariance
#'  [N_CovViews x 1]}}
#' 
#' And the returned Posterior distribution list includes the elements:
#' \itemize{
#' \item{Posterior$monitoring_time }{ monitoring times [t_*n_ x 1]}
#' \item{Posterior$dimension }{ labels of the risk drivers [t_*n_ x 1]}
#' \item{Posterior$cov }{ posterior covariance matrice of X_{t1,t2...t_}
#' [t_*n_ x t_*n_]}
#' \item{Posterior$mean }{ posterior vector of the means of X_{t1,t2...t_}
#' [t_*n_ x 1]}
#' \item{Posterior$mean_cost [t_*n_ x 1]}
#' \item{Posterior$mean_lin [t_*n_ x 1]}}
#' Posterior$mean_cost and Posterior$mean_lin are such that
#' Posterior$mean_cost + Posterior$mean_lin*x0  = Posterior$mean
#'
#' @references
#' A. Meucci - "Dynamic Portfolio Management with Views at Multiple Horizons"
#' \url{http://symmys.com/node/831}. See Meucci script for "MVOU_Prior.m"
#' 
#' @author Xavier Valls \email{xavievallspla@@gmail.com}
#' @export

MVOU_Posterior <- function(Mom, Views) {
  n <- unique(Mom$dimension)
  t <- unique(Mom$monitoring_time)
  n_ <- length(n)
  t_ <- length(t)
  grid <- meshgrid(t, 1:n_)
  T <- grid$X
  N <- grid$Y
  Posterior <- list()
  Posterior$monitoring_time <- array(T)
  Posterior$dimension <- array(N)
  Posterior$mean <- matrix(NaN, t_ * n_, 1)
  Posterior$cov <- matrix(NaN, n_ * t_, n_ * t_)
  Posterior$mean_cost <- matrix(NaN, t_ * n_, 1)
  Posterior$mean_lin <- matrix(NaN, t_ * n_, n_)

  S2 <- Mom$cov
  mu <- Mom$mean
  v_mu <- Views$v_mu
  v_sig <- Views$v_sig
  mu_view <- Views$mu_view
  sig2_view <- Views$sig2_view

  if (all(is.nan(Views$v_mu))) {
    Posterior$mean <- mu
  }else {
    v_mu_dag <- (S2 %*% t(v_mu)) / (v_mu %*% S2 %*% t(v_mu))[1]
    P_orth <- v_mu_dag %*% v_mu
    P <- diag(1, dim(P_orth)[1], dim(P_orth)[2]) - P_orth
    Posterior$mean <- P %*% mu + P_orth %*% v_mu_dag %*% mu_view
    Posterior$mean_lin <- P %*% Mom$mean_lin
    Posterior$mean_cost <- P %*% Mom$mean_cost + P_orth %*% v_mu_dag %*% mu_view
  }
  if (all(is.nan(Views$v_sig))) {
    Posterior$cov <- S2
  } else {
    v_sig_dag <- (S2 %*% t(v_sig)) / (v_sig %*% S2 %*% t(v_sig))[1]
    P_orth <- v_sig_dag %*% v_sig
    diag(1, dim(P_orth)[1], dim(P_orth)[2]) - P_orth
    Posterior$cov <- P %*% S2 %*% t(P) + P_orth %*% (v_sig_dag %*% sig2_view %*%
                                                     t(v_sig_dag)) %*% t(P_orth)
  }
  return(Posterior)
}
