#' Turnover constrained portfolio optimization
#' 
#' Calculate portfolio weights, variance, and mean return, given a set of 
#' returns and a constraint on overall turnover
#' 
#' 
#' @param returns an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param mu.target target portfolio return
#' @param w.initial initial vector of portfolio weights.  Length of the vector
#' must be equal to ncol(returns)
#' @param turnover constraint on turnover from intial weights
#' @param long.only optional long only constraint.  Defaults to FALSE
#' @return returns a list with initial weights, buys, sells, and
#' the aggregate of all three.  Also returns the portfolio's expected
#' return and variance
#' @author James Hobbs
#' @seealso \code{\link{TurnoverFrontier}}
#' @seealso \code{\link{solve.QP}} 
#' 
#' data(Returns) 
#'     opt <- TurnoverOpt(large.cap.returns,mu.target=0.01, 
#'      w.initial = rep(1/100,100),turnover=5) 
#'   		opt$w.total 
#' 			opt$port.var 
#'      opt$port.mu 
#' @export
TurnoverOpt <- function(returns,mu.target,w.initial,turnover, long.only = FALSE){
  nassets <- ncol(returns)
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
  constraint.sum <- c(rep(1,2*nassets),rep(1,nassets))
  constraint.mu.target <- mu
  constraint.weights.initial <- rbind(diag(nassets),matrix(0,ncol=nassets,nrow=nassets*2))
  #Make both w_buy and w_sell negative, and check that it is > the negative turnover
  constraint.turnover <- c(rep(0,nassets),rep(-1,nassets),rep(1,nassets))
  constraint.weights.positive <-
    rbind(matrix(0,ncol=2*nassets,nrow=nassets),diag(2*nassets))
  temp.index <- (nassets*3-nassets+1):(nassets*3)
  #need to flip sign for w_sell
  constraint.weights.positive[temp.index,]<-
    constraint.weights.positive[temp.index,]*-1
  
  #put left hand side of constraints into constraint matrix
  Amat <- cbind(constraint.sum, constraint.mu.target, constraint.weights.initial,
                constraint.turnover, constraint.weights.positive)
  #right hand side of constraints in this vector
  bvec <- c(1,mu.target,w.initial,-turnover,rep(0,2*nassets))
  
  #optional long only constraint
  if(long.only == TRUE){
    if ( length(w.initial[w.initial<0]) > 0 ){
      stop("Long-Only specified but some initial weights are negative")
    }
    constraint.long.only <- rbind(diag(nassets),diag(nassets),diag(nassets))
    Amat <- cbind(Amat, constraint.long.only)
    bvec <- c(bvec,rep(0,nassets))
  }
  
  #Note that the first 5 constraints are equality constraints
  #The rest are >= constraints, so if you want <= you have to flip
  #signs as done above
  solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=(2+nassets))
  
  port.var <- solution$value
  w.buy <- solution$solution[(nassets+1):(2*nassets)]
  w.sell <- solution$solution[(2*nassets+1):(3*nassets)]
  w.total <- w.initial + w.buy + w.sell
  achieved.turnover <- sum(abs(w.buy),abs(w.sell))
  port.mu <- w.total%*%(mu[1:nassets])
  list(w.initial = w.initial, w.buy = w.buy,w.sell=w.sell,
       w.total=w.total,achieved.turnover = achieved.turnover,
       port.var=port.var,port.mu=port.mu)
}



#' Turnover constrained portfolio frontier
#' 
#' Calculates an efficient frontier of portfolios with a 
#' constraint on overall turnover
#' 
#' @param returns an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param minmu min feasible target portfolio return to use in optimization
#' @param maxmu max feasible target portfolio return to use in optimization
#' @param w.initial initial vector of portfolio weights.  Length of the vector
#' must be equal to ncol(returns)
#' @param turnover constraint on turnover from intial weights
#' @param long.only optional long only constraint.  Defaults to FALSE
#' @return returns a matrix, with the first column of mean return
#' second column of portfolio standard deviation, and subsequent columns of
#' asset weights
#' @author James Hobbs
#' @seealso \code{\link{TurnoverOpt}}
#' 
#' data(Returns) 
#'  efront <- TurnoverFrontier(large.cap.returns,npoints=50,minmu=0.001, 
#'  maxmu=.05, w.initial=rep(1/100,100),turnover=5) 
#'  plot(x=efront[,"SD"],y=efront[,"MU"],type="l") 
#' @export
TurnoverFrontier <- function(returns,npoints = 10, minmu, maxmu,
                             w.initial,turnover,long.only = FALSE)
{
  p = ncol(returns)
  efront = matrix(rep(0,npoints*(p+2)),ncol = p+2)
  dimnames(efront)[[2]] = c("MU","SD",dimnames(returns)[[2]])
  muvals = seq(minmu,maxmu,length.out = npoints)
  for(i in 1:npoints)    {
    opt <- TurnoverOpt(returns,mu.target = muvals[i],w.initial,turnover,long.only)
    efront[i,"MU"] <- opt$port.mu
    efront[i,"SD"] <- sqrt(opt$port.var)
    efront[i,3:ncol(efront)] <- opt$w.total
  }
  
  efront
}



