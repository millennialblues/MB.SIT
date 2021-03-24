#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')
load.packages('kernlab')

##replace tickers here; e.g. UPRO = TQQQ; TMF = BND
tickers = '
UPRO = UPRO
TMF = TMF
'

tickers = spl('UPRO,TMF')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            

bt.prep(data, align='remove.na', fill.gaps = T, dates='2010:01::')


#ma.start.date = (start.date - 1)
#ma.all.date = paste(start.date, end.date, sep = '::')
  
#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices

models = list()

#*****************************************************************
# UPRO
#******************************************************************
data$weight[] = NA
data$weight$UPRO = 1
models$UPRO = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)

#*****************************************************************
# UPRO + 10 month go to cash filter
#******************************************************************
sma = bt.apply.matrix(prices, SMA, 10*22)

data$weight[] = NA
data$weight$UPRO = iif(prices$UPRO > sma$UPRO, 1, 0)
data$weight$TMF = 1 - ifna( ifna.prev(data$weight$UPRO), 0)
models$UPRO.TMF = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)

#*****************************************************************
# UPRO + 10 month +5/-5% go to cash filter
#******************************************************************
data$weight[] = NA
data$weight$UPRO = iif(cross.up(prices, sma * 1.05), 1, iif(cross.dn(prices, sma * 0.95), 0, NA))$UPRO
data$weight$TMF = 1 - ifna( ifna.prev(data$weight$UPRO), 0)
models$UPRO.TMF.BAND = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)


#*****************************************************************
# UPRO + dynamic TMF filter base on volatility
#******************************************************************
#ret = diff(log(prices))
#hist.vol = bt.apply.matrix(ret, runSD, n = 21)
#
#vol.rank = bt.apply.matrix(hist.vol, percent.rank, 252)
#
#sma.TMF = sma * NA
#sma.TMF[] = iif(vol.rank < 0.5, bt.apply.matrix(prices, SMA, 10*22), bt.apply.matrix(prices, SMA, 1*22))
#
#data$weight[] = NA
#data$weight$UPRO = iif(prices$UPRO <= sma$UPRO | prices$UPRO <= sma.TMF$UPRO, 0, iif(prices$UPRO > sma$UPRO, 1, NA))
#data$weight$TMF = 1 - ifna( ifna.prev(data$weight$UPRO), 0)
#models$UPRO.TMF.VOL.SIMPLE = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)

#*****************************************************************
# UPRO + dynamic TMF filter base on volatility; multiple levels
#******************************************************************
#nbreaks = 5
#map.index = seq(0,1, 1/nbreaks)
#map = bt.apply.matrix(vol.rank, function(x) as.numeric(cut(x, map.index)))

#sma.TMF = sma * NA
#for(i in 1:nbreaks) {
#  temp = coredata(bt.apply.matrix(prices, SMA, (nbreaks - i + 1)* 2 *22))
#  index = ifna(map == i, F)
#  sma.TMF[index] = temp[index]
#}

#data$weight[] = NA
#data$weight$UPRO = iif(prices$UPRO <= sma$UPRO | prices$UPRO <= sma.TMF$UPRO, 0, iif(prices$UPRO > sma$UPRO, 1, NA))
#data$weight$TMF = 1 - ifna( ifna.prev(data$weight$UPRO), 0)
#models$UPRO.TMF.VOL = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)


#data$weight[] = NA
#data$weight$UPRO = iif(prices$UPRO <= sma$UPRO | prices$UPRO <= sma.cash$UPRO, 0, iif(prices$UPRO > sma$UPRO, 1, NA))
#data$weight$TMF = 1 - ifna( ifna.prev(data$weight$UPRO), 0)
#models$UPRO.TMF.VOL = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)

#*****************************************************************
# Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)




#*****************************************************************
# Report
#*****************************************************************
#UPRO.visualize.signal('base', ma.all.date) #choose dates




#strategy.performance.snapshoot(models, T) #comparison snapshop

#plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
#mtext('Cumulative Performance', side = 2, line = 1)

plotbt.strategy.sidebyside(models, make.plot=T, return.table=T,perfromance.fn = engineering.returns.kpi)

plotbt.custom.report.part2(models$UPRO.TMF.BAND, trade.summary = TRUE) #pie chart, allocation, months, etc
plotbt.custom.report.part3(models$UPRO.TMF.BAND, trade.summary = TRUE)   #trade table
