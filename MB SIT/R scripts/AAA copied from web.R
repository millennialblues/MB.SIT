###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

tickers = spl('^RUT,^IXIC,FSPHX,XBI,^SOX,IAT,VGSIX,TLT')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)

#*****************************************************************
#leverage-up indexes!!
#*****************************************************************

#data$RUT <- create.leveraged(data$RUT, leverage=3)
#data$IXIC <- create.leveraged(data$IXIC, leverage=3)
#data$FSPHX <- create.leveraged(data$FSPHX, leverage=3)
#data$XBI <- create.leveraged(data$XBI, leverage=3)
#data$SOX <- create.leveraged(data$SOX, leverage=3)
#data$IAT <- create.leveraged(data$IAT, leverage=3)
#data$VGSIX <- create.leveraged(data$VGSIX, leverage=3)
#data$TLT <- create.leveraged(data$TLT, leverage=3)

for(i in ls(data)) data[[i]] = create.leveraged(data[[i]], 3)                            
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
bt.prep(data, align='keep.all', dates='2006:06::')


#*****************************************************************
# Code Strategies
#******************************************************************
prices = data$prices  
n = ncol(prices)

models = list()

# find period ends
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

# Adaptive Asset Allocation parameters
n.top = 4       # number of momentum positions
n.mom = 5*22    # length of momentum look back
n.vol = .5*22    # length of volatility look back   


#*****************************************************************
# Equal Weight
#******************************************************************
data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)   
models$equal.weight = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Volatliliy Position Sizing
#******************************************************************
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)

adj.vol = 1/hist.vol[period.ends,]

data$weight[] = NA
data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)    
models$volatility.weighted = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Momentum Portfolio
#*****************************************************************
momentum = prices / mlag(prices, n.mom)

data$weight[] = NA
data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)   
models$momentum = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

#*****************************************************************
# Combo: weight positions in the Momentum Portfolio according to Volatliliy
#*****************************************************************
weight = ntop(momentum[period.ends,], n.top) * adj.vol

data$weight[] = NA
data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)   
models$combo = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

#*****************************************************************   
# Adaptive Asset Allocation (AAA)
# weight positions in the Momentum Portfolio according to 
# the minimum variance algorithm
#*****************************************************************   
weight = NA * prices
weight[period.ends,] = ntop(momentum[period.ends,], n.top)

for( i in period.ends[period.ends >= n.mom] ) {
  hist = ret.log[ (i - n.vol + 1):i, ]
  
  # require all assets to have full price history
  include.index = count(hist)== n.vol      
  
  # also only consider assets in the Momentum Portfolio
  index = ( weight[i,] > 0 ) & include.index
  n = sum(index)
  
  if(n > 0) {                  
    hist = hist[ , index]
    
    # create historical input assumptions
    ia = create.historical.ia(hist, 252)
    s0 = apply(coredata(hist),2,sd)       
    ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
    
    # create constraints: 0<=x<=1, sum(x) = 1
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
    
    # compute minimum variance weights                          
    weight[i,] = 0        
    weight[i,index] = min.risk.portfolio(ia, constraints)
  }
}

# Adaptive Asset Allocation (AAA)
data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]   
models$aaa = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

#*****************************************************************
# Create Report
#******************************************************************    
models = rev(models)

#plotbt.custom.report.part1(models)       
#plotbt.custom.report.part2(models)       
#plotbt.custom.report.part3(models$aaa, trade.summary = TRUE)   
#plotbt.custom.report.part3(models$combo, trade.summary = TRUE)
#plotbt.custom.report.part3(models$momentum, trade.summary = TRUE)
    


