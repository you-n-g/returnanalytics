### get data
library(xts)
library(quantmod)
library(PerformanceAnalytics)

###TODO load from package
file.choose()
mktCap <- read.csv("N:\\School\\Summer 2012\\Data Set\\Market Cap Data\\mktCapSummary.csv",as.is=TRUE)
classifiers <- c("class.2011","class.12yrAvg","class.2yrAvg")
large <- 10000
mid <- 2000
small <- 250


### Classify tickers into market cap classes
#  mktCap > 10,000 large
#         > 2,000 and <10,000 mid
#         > 250   and <2,000 small
#         < 250 micro
for (i in 1:length(classifiers)){
  mktCap[mktCap[,1+i] >= large,classifiers[i]]   <- "large"
  mktCap[mktCap[,1+i] >= mid & mktCap[,1+i] < large ,classifiers[i]]   <- "mid"
  mktCap[mktCap[,1+i] >= small & mktCap[,1+i] < mid ,classifiers[i]]   <- "small"
  mktCap[mktCap[,1+i] <= small,classifiers[i]]   <- "micro"
}

#Test
mktCap[mktCap[,"Ticker"]=="AAPL",]

###TODO Select smaller subset of tickers (~100) per group