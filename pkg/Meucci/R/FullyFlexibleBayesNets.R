#' Input conditional views
#' 
#' statement: View(k).Who (e.g. [1 3])= View(k).Equal (e.g. {[2 3] [1 3 5]})
#' optional conditional statement: View(k).Cond_Who (e.g. [2]) = 
#'                                 View(k).Cond_Equal (e.g. {[1]})
#' amount of stress quantified as Prob(statement)<=View(k).v if View(k).sgn = 1
#'                                Prob(statement)>=View(k).v if View(k).sgn = -1
#' 
#' confidence in stress is quantified in View(k).c in (0,1)
#' 
#' @param View              TBD
#' @param X                 TBD
#'
#' @return A                TBD
#' @return b                TBD
#' @return g                TBD
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export

CondProbViews <- function(View, X) {
  # initialize parameters
  A <- matrix(, nrow = 0, ncol = nrow(X))
  b <- g <- matrix(, nrow = 0, ncol = 1)
  c = c()
  # for each view...
  for (k in 1:length(View)) {
    I_mrg <- (X[, 1] < Inf)

    for (s in 1:length(View[[k]]$Who)) {
      Who <- View[[k]]$Who[s]
      Or_Targets <- View[[k]]$Equal[[s]]
      I_mrg_or <- (X[, Who] > Inf)
      for (i in 1:length(Or_Targets)) {
        I_mrg_or <- I_mrg_or | (X[, Who] == Or_Targets[i])
        } # element-wise logical OR
      I_mrg <- I_mrg & I_mrg_or # element-wise logical AND
    }

    I_cnd <- (X[, 1] < Inf)
    # If length of Cond_Who is zero, skip
    if (length(View[[k]]$Cond_Who) != 0) {
      for (s in 1:length(View[[k]]$Cond_Who)) {
        Who <- View[[k]]$Cond_Who[s]
        Or_Targets <- View[[k]]$Cond_Equal[[s]]
        I_cnd_or <- (X[, Who] > Inf)
        for (i in 1:length(Or_Targets)) {
          I_cnd_or <- I_cnd_or | X[,Who] == Or_Targets[i]
        }
        I_cnd <- I_cnd & I_cnd_or
      }
    }

    I_jnt <- I_mrg & I_cnd

    if (!isempty(View[[k]]$Cond_Who)) {
      New_A <- View[[k]]$sgn %*% t((I_jnt - View[[k]]$v * I_cnd))
      New_b <- 0
    }
    else {
      New_A <- View[[k]]$sgn %*% t(I_mrg)
      New_b <- View[[k]]$sgn %*% View[[k]]$v
    }

    A <- rbind(A, New_A) # constraint for the conditional expectations...
    b <- rbind(b, New_b)
    g <- rbind(g, -log(1 - View[[k]]$c))
    c <- rbind(c,I_mrg)
  }
  return(list(A = A, b = b, g = g, c=c))
}

#' tweak a matrix
#' @param   A   matrix A consisting of inequality constraints (Ax <= b)
#' @param   b   matrix b consisting of inequality constraint vector b (Ax <= b)
#' @param   g   TODO: TBD
#'
#' @return db
#' @export
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}

Tweak <- function(A, b, g) {
  K <- nrow(A)
  J <- ncol(A)

  g_ <- rbind(g, matrix(0, J, 1))

  Aeq_ <- cbind(matrix(0, 1, K), matrix(1, 1, J))
  beq_ <- 1

  lb_ <- rbind(matrix(0, K, 1), matrix(0, J, 1))
  ub_ <- rbind(Inf * matrix(1, K, 1), matrix(1, J, 1))

  A_ <- cbind(-diag(1, K), A)
  b_ <- b

  # add lower-bound and upper-bound constraints
  A_ <- rbind(A_, -diag(1, ncol(A_)))
  b_ <- rbind(b_, matrix(0, ncol(A_), 1))

  x0 <- rep(1 / ncol(Aeq_), ncol(Aeq_))
  dim(A_)
  dim(b_)
  dim(g_)
  db_ <- solveLP(g_,A_,b_,Aeq_,beq_,lb_,ub_)
  db <- db_[1:K]

  return(db)
}


#' @title Takes a matrix of joint-scenario probability distributions and
#'        generates expectations, standard devation, and correlation matrix
#'        for the assets
#'
#' @description Takes a matrix of joint-scenario probability distributions and
#'              generates expectations, standard devation, and correlation
#'              matrix for the assets
#'
#' @param X     a matrix of joint-probability scenarios (rows are scenarios,
#'              columns are assets)
#' @param p     a numeric vector containing the probabilities for each of the
#'              scenarios in the matrix X
#'
#' @return means                 a numeric vector of the expectations
#'                              (probability weighted) for each asset
#' @return sd                    a numeric vector of standard deviations
#'                               corresponding to the assets in the covariance
#'                               matrix
#' @return correlationMatrix     the correlation matrix resulting from
#'                               converting the covariance matrix to a
#'                               correlation matrix
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export

ComputeMoments <- function(X, p) {
  N <- ncol(X)
  m <- t(X) %*% p
  Sm <- t(X) %*% (X * repmat(p, 1, N)) # repmat : repeats/tiles a matrix
  S <- Sm - m %*% t(m)
  C <- cov2cor(S) # the correlation matrix
  s <- sqrt(diag(S)) # the vector of standard deviations
  return(list(means = m, sd = s, correlationMatrix = C))
}
