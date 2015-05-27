#***************************************************************
# Jarque-Bera Test:
# It has to be checked Probability of null (H0) or (H1).
# (http://stats.stackexchange.com/questions/130368/why-do-i-get-this-p-value-doing-the-jarque-bera-test-in-r)
#***************************************************************
# Christofferson Backtest for Independence:
# VaR(excess_loss<=0)=[]; Does not make sense. It is still to be checked if it is as intended.
# if(excess.loss[i-1]<=0) if condition incomplete statement.
#***************************************************************
# Tests/Examples for profit.loss distribution and corresponding VaR and ETL
# still needs to be completed. Around 4 in Backtest do not have examples.
# It still has to be completed.
#***************************************************************
# Lopez Backtest:
# In Christofferson , excess.loss is defined as -profit.loss-VaR
# But in Lopez Backtest, profit.loss-VaR is used. It has to be checked.
#***************************************************************