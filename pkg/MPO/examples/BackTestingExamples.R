
##backtesting turnover example
wt.to <- BackTestWeights(large.cap.returns,12,36,FUN=TurnoverOpt,w.initial=rep(1/100,100),turnover=2)
class(wt.to)
ret.to <- Return.rebalancing(large.cap.returns, wt.to2)
charts.PerformanceSummary(ret.to2,main="MV Portfolio, turnover constrained")

