
UtilityMaximization <- function(returns,lambda,long.only = FALSE){
  nassets <- ncol(returns)
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat*lambda 
  mu <- apply(returns,2,mean)
  dvec <- -mu #linear part is to maximize mean return
  
  #left hand side of constraints
  constraint.weight.sum <- rep(1,nassets)
  Amat <- cbind(constraint.weight.sum)
  
  #right hand side of constraints
  bvec <- c(-1)
  
  #optional long only constraint
  if(long.only == TRUE){
    constraint.long.only <- -diag(nassets)
    Amat <- cbind(Amat, constraint.long.only)
    bvec <- c(bvec,rep(0,nassets))
    print("LONG ONLY")
  }
  
  solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=1)
  w <- -solution$solution
  port.var <- w%*%cov.mat%*%w
  port.mu <- w%*%mu
  list(w = w, port.var=port.var, port.mu=port.mu)  
}


#' Quadratic Portfolio Optimization with transaction costs
#' 
#' 2 step utility maximization including tranasaction costs as a penalty
#' 
#' @param returns an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param lambda a risk aversion parameter
#' @param w.initial initial vector of portfolio weights.  Length of the vector
#' must be equal to ncol(returns)
#' @param c transaction costs.  Must be a single value or a vector of length
#' equal to ncol(returns)
#' @param long.only optional long only constraint.  Defaults to FALSE
#' @return returns a list with portfolio weights, return, and variance
#' @author James Hobbs
#' @seealso \code{\link{TransCostFrontier}}
#' 
#'    data(Returns) 
#'     opt <- TransactionCostOpt(large.cap.returns,w.initial=rep(1/100,100), 
#'     lambda=1,c=.0005) 
#' @export
TransactionCostOpt <- function(returns,lambda,w.initial,c,long.only = FALSE,
                               niterations = 1,max.iter=10){
  nassets <- ncol(returns)
  if(length(c)==1){
    c = rep(c,nassets)
  }
  if(length(c)!=nassets){
    stop("c must either be a single value, or the same length as the number of assets")
  }
  #step 1: optimize without constraints to determine buys vs sells
  #if w*>w.initial c = c
  #if w* < w.initial c = -c
  
  unconstrained <- UtilityMaximization(returns,lambda,long.only)
  w.unconstrained <- unconstrained$w
  diff <- w.unconstrained-w.initial 
  
  sign.vec <- rep(1,nassets)
  sign.vec[diff<0] <- -1
  cost <- c*sign.vec
  
  #step 2: optimize with fixed c from step 1
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat*lambda
  mu <- apply(returns,2,mean)
  dvec <- -(mu-cost) #linear part is to maximize mean return

  #left hand side of constraints
  constraint.weight.sum <- rep(1,nassets)
  #constraint.weight.buysell <- -diag(sign.vec)
  #Amat <- cbind(constraint.weight.sum,constraint.weight.buysell)
  Amat <- cbind(constraint.weight.sum)
  
  #right hand side of constraints
  #bvec <- c(-1,(w.initial*sign.vec))
  bvec <- -1
  
  #optional long only constraint
  if(long.only == TRUE){
    if ( length(w.initial[w.initial<0]) > 0 ){
      stop("Long-Only specified but some initial weights are negative")
    }
    constraint.long.only <- -diag(nassets)
    Amat <- cbind(Amat, constraint.long.only)
    bvec <- c(bvec,rep(0,nassets))
  }
  
  
  curr.iter <- 1
  
  while (curr.iter <= max.iter){
    if(curr.iter == 1){
      prev.sign.vec <- sign.vec
    }else{
      cost <- c*sign.vec
      dvec <- -(mu-cost)
    }
    solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=1)
    w.curr <- -solution$solution
    diff <- w.curr - w.initial
    sign.vec <- rep(1,nassets)
    sign.vec[diff<0] <- -1
    if(isTRUE(all.equal(prev.sign.vec,sign.vec))){break}
    prev.sign.vec <- sign.vec
    curr.iter <- curr.iter + 1
  }
  if(curr.iter > max.iter){
    warning("Max iterations reached.  Transaction costs may be too high relative to returns.")
  }
  #solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=1)
  w <- -solution$solution
  port.var <- w%*%cov.mat%*%w
  port.mu <- w%*%mu
  list(w = w, port.var=port.var, port.mu=port.mu,w.unconstrained=w.unconstrained)
}


#' Transaction cost penalized portfolio efficient frontier
#' 
#' Calculates an efficient frontier of portfolios using transaction costs
#' as a penalty.
#' 
#' @param returns an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param min.lambda minimum feasible risk aversion parameter to use in optimization
#' @param max.lambda maximum feasible risk aversion parameter to use in optimization
#' @param w.initial initial vector of portfolio weights.  Length of the vector
#' must be equal to ncol(returns)
#' @param c transaction costs.  Must be a single value or a vector of length
#' equal to ncol(returns)
#' @param long.only optional long only constraint.  Defaults to FALSE
#' @return returns a matrix, with the first column of mean return
#' second column of portfolio standard deviation, and subsequent columns of
#' asset weights
#' @author James Hobbs
#' @seealso \code{\link{TransactionCostOpt}}
#' 
#'  data(Returns) 
#'    efront <- TransCostFrontier(large.cap.returns,npoints=50,min.lambda=5, 
#'    max.lambda=1000,w.initial=rep(1/100,100),c=0.0005) 
#'    plot(x=efront[,"SD"],y=efront[,"MU"],type="l") 
#'  @export
TransCostFrontier <- function(returns,npoints = 10, min.lambda, max.lambda,
                              w.initial,c,long.only = FALSE)
{
  p = ncol(returns)
  efront = matrix(rep(0,npoints*(p+2)),ncol = p+2)
  dimnames(efront)[[2]] = c("MU","SD",dimnames(returns)[[2]])
  lambda.vals = seq(min.lambda,max.lambda,length.out = npoints)
  for(i in 1:npoints)    {
    opt <- TransactionCostOpt(returns,lambda = lambda.vals[i],w.initial,c,long.only)
    #opt <- UtilityMaximization(returns,lambda = lambda.vals[i])
    efront[i,"MU"] <- opt$port.mu
    efront[i,"SD"] <- sqrt(opt$port.var)
    efront[i,3:ncol(efront)] <- opt$w
  }
  
  efront
}

