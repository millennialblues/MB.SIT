###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
load(SIT)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

# funds
tickers = spl('TNA,TQQQ,TMF,CURE,LABU,SOXL,DPST,DRN') 

start.date = 1998

dates = paste(start.date,'::',sep='') 

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates=paste(start.date-2,':12::',sep=''), fill.gaps = T)

#*****************************************************************
# Setup
#******************************************************************         
prices = data$prices   
n = ncol(prices)
nperiods = nrow(prices)

periodicity = 'months'
period.ends = endpoints(prices, periodicity)
period.ends = period.ends[period.ends > 0]

max.product.exposure = 0.6  

#*****************************************************************
# Input Assumptions
#******************************************************************     
lookback.len = 40
create.ia.fn = create.ia

# input assumptions are averaged on 20, 40, 60 days using 1 day lag
ia.array = c(20,40,60)
avg.create.ia.fn = create.ia.averaged(ia.array, 1)

#*****************************************************************
# Momentum
#******************************************************************     
universe = prices > 0

mom.lookback.len = 120  
momentum = prices / mlag(prices, mom.lookback.len) - 1
mom.universe = ifna(momentum > 0, F)

# momentum is averaged on 20,60,120,250 days using 3 day lag
mom.array = c(20,60,120,250)    
avg.momentum = momentum.averaged(prices, mom.array, 3)
avgmom.universe = ifna(avg.momentum > 0, F)

#*****************************************************************
# Algos
#******************************************************************     
min.risk.fns = list(
  EW = equal.weight.portfolio,
  MV = min.var.portfolio,
  MCE = min.corr.excel.portfolio,
  
  MV.RSO = rso.portfolio(min.var.portfolio, 3, 100, const.ub = max.product.exposure),
  MCE.RSO = rso.portfolio(min.corr.excel.portfolio, 3, 100, const.ub = max.product.exposure)
)

#*****************************************************************
# Code Strategies
#******************************************************************     
make.strategy.custom <- function(name, create.ia.fn, lookback.len, universe, env) {
  obj = portfolio.allocation.helper(data$prices, 
                                    periodicity = periodicity,
                                    universe = universe,
                                    lookback.len = lookback.len,
                                    create.ia.fn = create.ia.fn,
                                    const.ub = max.product.exposure,
                                    min.risk.fns = min.risk.fns,
                                    adjust2positive.definite = F
  )
  env[[name]] = create.strategies(obj, data, prefix=paste(name,'.',sep=''))$models
}


models <- new.env()  
make.strategy.custom('ia.none'        , create.ia.fn    , lookback.len, universe       , models)
make.strategy.custom('ia.mom'         , create.ia.fn    , lookback.len, mom.universe   , models)
make.strategy.custom('ia.avg_mom'     , create.ia.fn    , lookback.len, avgmom.universe, models)
make.strategy.custom('avg_ia.none'    , avg.create.ia.fn, 252         , universe       , models)
make.strategy.custom('avg_ia.mom'     , avg.create.ia.fn, 252         , mom.universe   , models)
make.strategy.custom('avg_ia.avg_mom' , avg.create.ia.fn, 252         , avgmom.universe, models)

#*****************************************************************
# Create Report
#*****************************************************************      
strategy.snapshot.custom <- function(models, n = 0, title = NULL) {
  if (n > 0)
    models = models[ as.vector(matrix(1:len(models),ncol=n, byrow=T)) ] 
  
  layout(1:3) 
  plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
  mtext('Cumulative Performance', side = 2, line = 1)
  plotbt.strategy.sidebyside(models)
  barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', T) 
}

# basic vs basic + momentum => momentum filter has better results
models.final = c(models$ia.none, models$ia.mom)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Momentum Filter')

# basic vs basic + avg ia => averaged ia reduce turnover
models.final = c(models$ia.none, models$avg_ia.none)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Averaged Input Assumptions')

# basic + momentum vs basic + avg.momentum => mixed results for averaged momentum
models.final = c(models$ia.mom, models$ia.avg_mom)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Averaged Momentum')

# basic + momentum vs avg ia + avg.momentum
models.final = c(models$ia.mom, models$avg_ia.avg_mom)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Averaged vs Base')   