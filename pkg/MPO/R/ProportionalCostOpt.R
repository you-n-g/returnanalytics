#' Proportional cost portfolio optimization
#' 
#' Calculate portfolio weights, variance, and mean return, given a set of 
#' returns and a value for proportional transaction costs
#' 
#' 
#' @param returns an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param mu.target target portfolio return
#' @param w.initial initial vector of portfolio weights.  Length of the vector
#' must be equal to ncol(returns)
#' @param tc proportional transaction cost
#' @param long.only optional long only constraint.  Defaults to FALSE
#' @return returns a list with initial weights, buys, sells, and
#' the aggregate of all three.  Also returns the portfolio's expected
#' return and variance
#' @author James Hobbs
#' @seealso \code{\link{TurnoverFrontier}}
#' @seealso \code{\link{solve.QP}} 
#' 
#' data(Returns) 
#'     opt <- ProportionalCostOpt(large.cap.returns,mu.target=0.004, 
#'      w.initial = rep(1/100,100),tc=.01) 
#'     	opt$w.total 
#' 			opt$port.var 
#'      opt$port.mu 
#' @export
ProportionalCostOpt <- function(returns,mu.target,w.initial,tc,long.only = FALSE){
  nassets <- ncol(returns)
  if(length(tc)==1){
    tc = rep(tc,nassets)
  }
  if(length(tc)!=nassets){
    stop("tc must either be a single value, or the same length as the number of assets")
  }
  #using 3 sets of variabes...w.initial, w.buy, and w.sell
  returns <- cbind(returns,returns,returns)
  #The covariance matrix will be 3Nx3N rather than NxN
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat
  #Make covariance positive definite
  #This should barely change the covariance matrix, as
  #the last few eigen values are very small negative numbers
  Dmat <- make.positive.definite(Dmat)
  mu <- apply(returns,2,mean)
  dvec <- rep(0,nassets*3) #no linear part in this problem
  
  #left hand side of constraints
  constraint.sum <- c(rep(1,nassets),1+tc,(1-tc))
  #constraint.sum <- c(rep(1,2*nassets),rep(1,nassets))
  constraint.mu.target <- (1+mu)
  constraint.weights.initial <- rbind(diag(nassets),matrix(0,ncol=nassets,nrow=nassets*2))
  constraint.weights.positive <-
    rbind(matrix(0,ncol=2*nassets,nrow=nassets),diag(2*nassets))
  temp.index <- (nassets*3-nassets+1):(nassets*3)
  #need to flip sign for w_sell
  constraint.weights.positive[temp.index,]<-
    constraint.weights.positive[temp.index,]*-1
  

    #put left hand side of constraints into constraint matrix
    Amat <- cbind(constraint.sum, constraint.mu.target, constraint.weights.initial,
                  constraint.weights.positive)
    
    #right hand side of constraints in this vector
    bvec <- c(1,(1+mu.target),w.initial,rep(0,2*nassets))
    n.eq = 2+ nassets

 
  #optional long only constraint
  if(long.only == TRUE){
    if ( length(w.initial[w.initial<0]) > 0 ){
      stop("Long-Only specified but some initial weights are negative")
    }
    constraint.long.only <- rbind(diag(nassets),diag(nassets),diag(nassets))
    Amat <- cbind(Amat, constraint.long.only)
    bvec <- c(bvec,rep(0,nassets))
  }
  
  
  
  solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=n.eq)
  
  port.var <- solution$value
  w.buy <- solution$solution[(nassets+1):(2*nassets)]
  w.sell <- solution$solution[(2*nassets+1):(3*nassets)]
  w.total <- w.initial + w.buy + w.sell
  port.mu <- w.total%*%(mu[1:nassets])
  list(w.initial = w.initial, w.buy = w.buy,w.sell=w.sell,
       w=w.total, port.var=port.var,port.mu=port.mu)
}


