###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
library(SIT)

#*****************************************************************
# Load historical data
#******************************************************************  
load.packages('quantmod')  

tickers = spl('UPRO')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data)

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   

models = list()

#*****************************************************************
# Code Strategies
#****************************************************************** 
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)

#*****************************************************************
# Code Strategies : MA Cross Over
#****************************************************************** 
sma.fast = SMA(prices, 20)
sma.slow = SMA(prices, 50)

buy.signal = iif(cross.up(sma.fast, sma.slow), 1, NA)

data$weight[] = NA
data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
models$ma.cross = bt.run.share(data, clean.signal=T, trade.summary = TRUE)



#*****************************************************************
# Exit using fixed stop
#****************************************************************** 
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), fixed.stop, 
                               pstop = 1/100)
models$ma.cross.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Exit using trailing stop
#****************************************************************** 
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop, 
                               pstop = 1/100)
models$ma.cross.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Exit using trailing stop or profit target
#******************************************************************         
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop.profit.target, 
                               pstop = 1/100, pprofit = 1.5/100)
models$ma.cross.trailing.stop.profit.target = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#*****************************************************************
# Create Report
#******************************************************************     
strategy.performance.snapshoot(models, T)