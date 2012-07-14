#classic mv
#From Guy Yollins code
MinVariancePortOpt = function(returns)
{
  C <- var(returns)
  one <- rep(1, nrow(C))
  z <- solve(C, one)
  # Compute z = C.inv * 1
  cc <- t(one) %*% z
  # Compute cc = 1.transpose * C.inv * 1
  cc <- as.numeric(cc)
  # Convert 1-by-1 matrix to a scalar
  w <- z/cc
  mu <- apply(returns, 2, mean)
  a <- t(mu) %*% z
  port.mu <- as.numeric(a/cc)
  port.sd <- 1/cc^0.5
  list(w = w, port.mu = port.mu, port.sd = port.sd)
}


PortOptUnconstrained <- function(returns,mu.target){
  nassets <- ncol(returns)
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat
  mu <- apply(returns,2,mean)
  dvec <- rep(0,nassets) #no linear part
  constraint.sum <- c(rep(1,nassets))
  constraint.mu.target <- mu
  Amat <- cbind(constraint.sum, constraint.mu.target)
  bvec <- c(1,mu.target)
  solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=(2))
  port.sd <- (solution$value)^0.5
  w <- solution$solution
  port.mu <- w%*%mu
  list(w = w, port.sd = port.sd, port.mu = port.mu)
}

UnconstrainedFrontier <- function(returns,npoints = 10){
  nassets <- ncol(returns)
  min.variance.port <- MinVariancePortOpt(returns)
  mu.max <- max(apply(returns,2,mean))
  mu.min <- min.variance.port$port.mu
  mu.vals <- seq(mu.min, mu.max, length.out = npoints)
  result = matrix (0,nrow = npoints, ncol = (nassets+2))
  colnames(result)=c("MU","SD",colnames(returns))
  for(i in 1:npoints){
    sol <- PortOptUnconstrained(returns,mu.vals[i])
    result[i,"MU"] <- sol$port.mu
    result[i,"SD"] <- sol$port.sd
    result[i,3:(nassets+2)] <- sol$w
  }
  result  
}

