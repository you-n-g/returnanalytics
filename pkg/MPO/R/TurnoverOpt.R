library(xts)
library(quadprog)
library(corpcor)

#todo: documentation with Roxygen2 and updating package dependecies

TurnoverOpt <- function(returns,mu.target,w.initial,turnover){
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

  #Note that the first 5 constraints are equality constraints
  #The rest are >= constraints, so if you want <= you have to flip
  #signs as done above
  solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=(2+nassets))
  
  port.var <- solution$value
  w.buy <- solution$solution[(nassets+1):(2*nassets)]
  w.sell <- solution$solution[(2*nassets+1):(3*nassets)]
  w.total <- w.initial + w.buy + w.sell
  achieved.turnover <- sum(abs(w.buy),abs(w.sell))
  
  list(w.initial = w.initial, w.buy = w.buy,w.sell=w.sell, 
       w.total=w.total,achieved.turnover = achieved.turnover, port.var=port.var)
} 

##example
p = TurnoverOpt(large.cap.returns,mu.target=0.01,w.initial=rep(1/100,100),turnover=1)
p
sum(p$w.total)
sum(p$w.initial)
sum(p$w.buy)
sum(p$w.sell)

wt<-p$w.total
wt%*%cov(returns)%*%wt

