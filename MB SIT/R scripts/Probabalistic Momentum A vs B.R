###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
library(SIT)
#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

tickers = spl('UPRO,TMF')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='2009::')


#*****************************************************************
# Setup
#****************************************************************** 
lookback.len = 60

prices = data$prices

models = list()

#*****************************************************************
# Simple Momentum
#****************************************************************** 
momentum = prices / mlag(prices, lookback.len)

data$weight[] = NA
data$weight$UPRO[] = momentum$UPRO > momentum$TMF
data$weight$TMF[] = momentum$UPRO <= momentum$TMF
models$Simple  = bt.run.share(data, clean.signal=T)   

#*****************************************************************
# Probabilistic Momentum
#****************************************************************** 
confidence.level = 60/100
ret = prices / mlag(prices) - 1 

ir = sqrt(lookback.len) * runMean(ret$UPRO - ret$TMF, lookback.len) / runSD(ret$UPRO - ret$TMF, lookback.len)
momentum.p = pt(ir, lookback.len - 1)

data$weight[] = NA
data$weight$UPRO[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.dn(momentum.p, (1 - confidence.level)), 0,NA))
data$weight$TMF[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic  = bt.run.share(data, clean.signal=T) 

#*****************************************************************
# Probabilistic Momentum + UPRO Leverage 
#****************************************************************** 
#data$weight[] = NA
#data$weight$UPRO[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.up(momentum.p, (1 - confidence.level)), 0,NA))
#data$weight$TMF[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
#models$Probabilistic.Leverage = bt.run.share(data, clean.signal=T)  

#*****************************************************************
# Create Report
#******************************************************************    
strategy.performance.snapshoot(models, T)


#*****************************************************************
# Visualize Signal
#******************************************************************        
cols = spl('steelblue1,steelblue')
prices = scale.one(data$prices)

layout(1:3)

plota(prices$UPRO, type='l', ylim=range(prices), plotX=F, col=cols[1], lwd=2)
plota.lines(prices$TMF, type='l', plotX=F, col=cols[2], lwd=2)
plota.legend('UPRO,TMF',cols,as.list(prices))

highlight = models$Probabilistic$weight$UPRO > 0
plota.control$col.x.highlight = iif(highlight, cols[1], cols[2])
plota(models$Probabilistic$equity, type='l', plotX=F, x.highlight = highlight | T)
plota.legend('Probabilistic,UPRO,TMF',c('black',cols))

highlight = models$Simple$weight$UPRO > 0
plota.control$col.x.highlight = iif(highlight, cols[1], cols[2])
plota(models$Simple$equity, type='l', plotX=T, x.highlight = highlight | T)
plota.legend('Simple,UPRO,TMF',c('black',cols))  
