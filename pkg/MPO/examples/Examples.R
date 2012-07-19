source("TurnoverOpt.R")
source("TransactionOpt.R")
source("ClassicMV.R")


pdf("Charts.pdf")
#verify 3 methods are equal with no transaction cost/turnover
classicMV <- UnconstrainedFrontier(large.cap.returns,npoints=50)
trans.cost.front <- TransCostFrontier(large.cap.returns,npoints=50,min.lambda=5,max.lambda=1000,
                                      w.initial=rep(1/100,100),c=0)
turnover.front <- TurnoverFrontier(large.cap.returns,npoints=50,minmu=0.001,maxmu=.1,
                                   w.initial=rep(1/100,100),turnover=1000)

plot(x=classicMV[,"SD"],y=classicMV[,"MU"],type="l",col="black",main="Comparison with no Turnover/TC constraints"
     ,xlab="SD",ylab="MU")
lines(x=trans.cost.front[,"SD"],y=trans.cost.front[,"MU"],lwd=2,lty=2,col="blue")
lines(x=turnover.front[,"SD"],y=turnover.front[,"MU"],lwd=3,lty=2,col="red")
legend("topleft",legend = c("Classic MV","Trans Cost Penalty", "Turnover Constraint"),
       col=c("black","blue","red"),lty=1:3,bty="n")

#low penalty
trans.cost.front.low <- TransCostFrontier(large.cap.returns,npoints=50,min.lambda=5,max.lambda=1000,
                                          w.initial=rep(1/100,100),c=0.0005)
turnover.front.low <- TurnoverFrontier(large.cap.returns,npoints=50,minmu=0.001,maxmu=.05,
                                       w.initial=rep(1/100,100),turnover=5)
plot(x=classicMV[,"SD"],y=classicMV[,"MU"],type="l",col="black",main="Comparison with low Turnover/TC constraints"
     ,xlab="SD",ylab="MU")
lines(x=trans.cost.front.low[,"SD"],y=trans.cost.front.low[,"MU"],lwd=2,lty=2,col="blue")
lines(x=turnover.front.low[,"SD"],y=turnover.front.low[,"MU"],lwd=3,lty=2,col="red")
legend("topleft",legend = c("Classic MV","Trans Cost Penalty: 0.0005", "Turnover Constraint: 5"),
       col=c("black","blue","red"),lty=1:3,bty="n")


#med penalty
trans.cost.front.med <- TransCostFrontier(large.cap.returns,npoints=50,min.lambda=5,max.lambda=1000,
                                          w.initial=rep(1/100,100),c=0.001)
turnover.front.med <- TurnoverFrontier(large.cap.returns,npoints=50,minmu=0.001,maxmu=.05,
                                       w.initial=rep(1/100,100),turnover=4)
plot(x=classicMV[,"SD"],y=classicMV[,"MU"],type="l",col="black",main="Comparison with med Turnover/TC constraints",
     xlab="SD",ylab="MU")
lines(x=trans.cost.front.med[,"SD"],y=trans.cost.front.med[,"MU"],lwd=2,lty=2,col="blue")
lines(x=turnover.front.med[,"SD"],y=turnover.front.med[,"MU"],lwd=3,lty=2,col="red")
legend("topleft",legend = c("Classic MV","Trans Cost Penalty: 0.0010", "Turnover Constraint: 4"),
       col=c("black","blue","red"),lty=1:3,bty="n")

#high penalty

trans.cost.front.high <- TransCostFrontier(large.cap.returns,npoints=50,min.lambda=5,max.lambda=1000,
                                           w.initial=rep(1/100,100),c=0.005)
turnover.front.high <- TurnoverFrontier(large.cap.returns,npoints=50,minmu=0.001,maxmu=.05,
                                        w.initial=rep(1/100,100),turnover=3)
plot(x=classicMV[,"SD"],y=classicMV[,"MU"],type="l",col="black",main="Comparison with high Turnover/TC constraints",
     xlab="SD",ylab="MU")
lines(x=trans.cost.front.high[,"SD"],y=trans.cost.front.high[,"MU"],lwd=2,lty=2,col="blue")
lines(x=turnover.front.high[,"SD"],y=turnover.front.high[,"MU"],lwd=3,lty=2,col="red")
legend("topleft",legend = c("Classic MV","Trans Cost Penalty: 0.0050", "Turnover Constraint: 3"),
       col=c("black","blue","red"),lty=1:3,bty="n")
dev.off()


###############turnover##########

#long and short example
data("Returns.RData")
port.turn.10 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.1,
                                 w.initial=rep(1/100,100),turnover=10)
port.turn.2 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.05,
                                w.initial=rep(1/100,100),turnover=2)
port.turn.1 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.025,
                                w.initial=rep(1/100,100),turnover=1)
port.turn.05 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.016,
                                 w.initial=rep(1/100,100),turnover=.5)

plot(x=port.turn.10[,"SD"],y=port.turn.10[,"MU"],type="l",
     main="Efficent Frontiers with Turnover Constraints",xlab="SD",ylab="MU")
lines(x=port.turn.2[,"SD"],y=port.turn.2[,"MU"],col="blue")
lines(x=port.turn.1[,"SD"],y=port.turn.1[,"MU"],col="red")
lines(x=port.turn.05[,"SD"],y=port.turn.05[,"MU"],col="orange")
legend("topleft",bty="n",legend = c("Turnover 10","Turnover 2"
                                    ,"Turnover 1","Turnover .5"), col=c("black",
                                                                        "blue","red","orange"),lty=1)

#long only example
port.turn.long.10 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.020,
                                      w.initial=rep(1/100,100),turnover=10,long.only=TRUE)
port.turn.long.2 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.020,
                                     w.initial=rep(1/100,100),turnover=2,long.only=TRUE)
port.turn.long.1 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.015,
                                     w.initial=rep(1/100,100),turnover=1,long.only=TRUE)
port.turn.long.05 <- efrontMVTurnover(large.cap.returns,npoints=20,minmu=0.001,maxmu=.012,
                                      w.initial=rep(1/100,100),turnover=.5,long.only=TRUE)

plot(x=port.turn.long.10[,"SD"],y=port.turn.long.10[,"MU"],type="l",
     main="Long-Only Efficent Frontiers with Turnover Constraints",xlab="SD",ylab="MU")
lines(x=port.turn.long.2[,"SD"],y=port.turn.long.2[,"MU"],col="blue")
lines(x=port.turn.long.1[,"SD"],y=port.turn.long.1[,"MU"],col="red")
lines(x=port.turn.long.05[,"SD"],y=port.turn.long.05[,"MU"],col="orange")
legend("topleft",bty="n",legend = c("Turnover 10","Turnover 2",
                                    "Turnover 1","Turnover .5"), col=c("black",
                                                                       "blue","red","orange"),lty=1)
##############################