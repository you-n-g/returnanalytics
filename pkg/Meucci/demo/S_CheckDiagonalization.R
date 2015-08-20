#' This script verifies the correctness of the eigenvalue-eigenvector
#' representation in terms of real matrices for the transition matrix of an OU
#' process.
#'
#' A. Meucci - "Review of statistical arbitrage, cointegration, and multivariate
#' Ornstein-Uhlenbeck", 2009. \url{http://symmys.com/node/132}
#'
#' @author Manan Shah \email{mkshah@@cmu.edu}

N <- 5
Theta <- matrix(runif(N ^ 2), 5, byrow = T)

tmp <- eigen(Theta)
B <- tmp$vectors
L <- tmp$values

A <- Re(B) - Im(B)

Index_j <- as.matrix(which(Im (L) != 0))
L <- diag(L)
  
G <- L
for (s in c(seq(from = 1, to = length(Index_j), by = 2))) {
  G[Index_j[s:(s + 1)], Index_j[s:(s + 1)]] <- rbind(c(1, 0), c(0, 1)) *
  											  Re(L[Index_j[s],Index_j[s]]) +
  											  rbind(c(0, 1), c(-1, 0)) *
   											  Im(L [Index_j[s], Index_j[s]])
}
Theta_ <- A %*% G %*% solve(A)
