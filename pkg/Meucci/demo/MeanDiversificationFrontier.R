# This script computes the mean-diversification efficient frontier
#'
#' @references 
#' A. Meucci, "Managing Diversification". \url{http://www.symmys.com/node/199}
#' 
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}

# inputs
# upload returns covariance and expectations

# define benchmark and portfolio weights
N <- nrow(Mu)
w_0 <- rep(1, N) / N

# define constraints
# long-short constraints...
Constr <- list()
Constr$A <- rbind(diag(N), -diag(N))
Constr$b <- rbind(as.matrix(rep(1, N)), as.matrix(rep(0.1, N)))
Constr$Aeq <- t(as.matrix(rep(1, N))) # budget constraint...
Constr$beq <- as.matrix(1)

# mean-diversification analysis and frontier
EntropyFrontier <- MeanTCEntropyFrontier(S, Mu, w_b, w_0, Constr)

# mean-diversification of current allocation
m <- t(Mu) %*% (w_0 - w_b)
s <- sqrt(t(w_0 - w_b) %*% S %*% (w_0 - w_b))
GenPCResult <- GenPCBasis(S, emptyMatrix)
v_tilde <- G %*% (w_0 - w_b)
TE_contr <- (v_tilde * v_tilde) / s
R_2 <- max(10 ^ (-10), TE_contr / sum(TE_contr))
Ne <- exp(-t(R_2) %*% log(R_2))
