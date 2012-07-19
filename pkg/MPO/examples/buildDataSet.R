### get data
library(xts)
library(quantmod)
library(PerformanceAnalytics)

###TODO load from package
### market cap data from:  http://pages.stern.nyu.edu/~adamodar/New_Home_Page/data.html

#yahoo is missing data from DF and DFG
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

### Get 100 tickers by group
groupSize <- 100
large.tickers <- mktCap[mktCap[,"class.2011"]=="large","Ticker"][1:groupSize]
mid.tickers <- mktCap[mktCap[,"class.2011"]=="mid","Ticker"][1:groupSize]
small.tickers <- mktCap[mktCap[,"class.2011"]=="small","Ticker"][1:groupSize]
micro.tickers <- mktCap[mktCap[,"class.2011"]=="micro","Ticker"][1:groupSize]
micro.tickers <- mktCap[mktCap[,"class.2011"]=="micro","Ticker"][201:301]

### Download Data
start.date <- as.Date("2000-01-01")
end.date <- as.Date("2012-06-29")


###get symbols wrapper function
###add documentation and option for daily or weekly returns
getReturns <- function(symbols,from,to){
  nSymbols <- length(symbols)
  #check to see if data exists far back enough
  time <- paste(start.date,end.date,sep="/")
  dates <- timeBasedSeq(time)
  dates <- to.monthly(dates)
  ndates <- nrow(dates)
  check.df <- cbind(as.data.frame(symbols),valid=rep(FALSE,nSymbols))
  price.list <- list()
  for (i in 1:nSymbols){
    #download adjusted close
    currentSymbol <- getSymbols(symbols[i],auto.assign = FALSE 
                                          ,from=from,to=to)[,6]
    currentSymbol <- to.monthly(currentSymbol,indexAt="Date")[,4]
    if(nrow(currentSymbol) == ndates){
      #keep month end price
      price.list[[length(price.list)+1]] <- currentSymbol #not optimal but this loop is slow anyways
      check.df[i,"valid"] <- TRUE
      print("TRUE")
    }else{
      check.df[i,"valid"] <- FALSE #redundant
      print("FALSE")
    }
  }
  price.df = as.data.frame(price.list)
  price.xts = as.xts(price.df, order.by=as.Date(rownames(price.df)))
  returns <- Return.calculate(price.xts,method="compound")
  returns <- returns[-1]
  names(returns) <- check.df[check.df[,"valid"]==TRUE,1]
  list(returns=returns,check.df=check.df)
  
}

large.cap.returns <- getReturns(large.tickers,from = start.date, to = end.date)
mid.cap.returns <- getReturns(mid.tickers,from = start.date, to = end.date)
small.cap.returns <- getReturns(small.tickers,from = start.date, to = end.date)
micro.cap.returns <- getReturns(micro.tickers[11:20],from = start.date, to = end.date)
  micro.cap.returns$returns

time = paste(start.date,end.date,sep="/")
time
a=timeBasedSeq(time)
b=to.monthly(a)
nrow(b)
length(a)

a=getSymbols("HOOK",auto.assign = FALSE,from=start.date,to=end.date)[,6]

micro.tickers[17]
head(a)

