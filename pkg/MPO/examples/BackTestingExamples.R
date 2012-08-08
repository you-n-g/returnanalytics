###only 149 data points, so may want to use less than 100 assets
returns <- cbind(large.cap.returns[,1:5],mid.cap.returns[,1:5])

##backtesting turnover example
wt.to <- BackTestWeights(returns,1,36,FUN=TurnoverOpt,w.initial=rep(1/10,10),turnover=.75)
ret.to <- Return.rebalancing(returns, wt.to)
charts.PerformanceSummary(ret.to,main="MV Portfolio, turnover constrained")

##backtesting proportional cost example
wt.pc <- BackTestWeights(returns,1,36,FUN=ProportionalCostOpt,w.initial=rep(1/10,10),tc=.001,mu.target=0.001)
ret.pc <- Return.rebalancing(returns,wt.pc)
charts.PerformanceSummary(ret.pc,main="MV Portfolio, Proportional Transaction Costs")

##backtesting fixed transaction cost
wt.tc <- BackTestWeights(returns,1,36,FUN=TransactionCostOpt,w.initial=rep(1/10,10),c=.01,lambda=1000)
ret.tc <- Return.rebalancing(returns,wt.tc)
charts.PerformanceSummary(ret.pc,main="MV Portfolio, Fixed Transaction Costs")

combined <- cbind(ret.to,ret.pc,ret.tc)
colnames(combined) <- c("Turnover","Proportional Cost","Fixed Cost")
charts.PerformanceSummary(combined)


