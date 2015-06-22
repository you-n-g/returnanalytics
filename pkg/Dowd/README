#
# General Notes for Modification: 
#***************************************************************
# FrechetVaR does not use hp and the remark about return value when it is vector is vaccuous.
#***************************************************************
# In Normal/t QQ Plots, dowd code does not work for matrices but the code contains parts that
# work for matrices. some vectors like pvec are not defined anywhere in his code.
#***************************************************************
# Some error is present in GumbelCopulaVaR and needs correction
#***************************************************************
# Bootstrap is functional (but HSVaR still does not accept matrix P/L
# and only still accepts vectors, its needs to be modified)
#***************************************************************
# Jarque-Bera Test:
# It has to be checked Probability of null (H0) or (H1).
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