###############################################################################
# A Quantitative Approach to Tactical Asset Allocation by M. Faber (2006)
# http://www.mebanefaber.com/timing-model/
#' @export 
###############################################################################
timing.strategy <- function
(
  tickers = spl('DIA,SPY,SHY'),
  dates = '1900::',
  periodicity = 'months',
  ma.len = 200,
  cash = 'SHY'	
) 
{
  #*****************************************************************
  # Load historical data 
  #****************************************************************** 
  data = strategy.load.historical.data(tickers, dates)
  
  prices = data$prices   
  n = ncol(prices)
  nperiods = nrow(prices)
  
  # find period ends
  period.ends = endpoints(data$prices, periodicity)
  period.ends = period.ends[period.ends > 0]
  
  
  models = list()
  
  #*****************************************************************
  # Code Strategies
  #****************************************************************** 	
  # ignore cash when selecting funds
  position.score = prices
  position.score$SHY = NA
  
  # Equal Weight
  weight = ntop(position.score[period.ends,], n)	
  
  data$weight[] = NA
  data$weight[period.ends,] = weight
  models$equal.weight = bt.run.share(data, clean.signal=F)
  
  
  # BuyRule, price > 10 month SMA
  sma = bt.apply.matrix(prices, SMA, ma.len)
  buy.rule = prices > sma
  buy.rule = ifna(buy.rule, F)
  
  weight = ntop(position.score[period.ends,], n)
  # keep in cash the rest of the funds
  weight[!buy.rule[period.ends,]] = 0
  weight$SHY = 1 - rowSums(weight)
  
  data$weight[] = NA
  data$weight[period.ends,]  = weight
  models$timing = bt.run.share(data, clean.signal=F, trade.summary=T)
  
  return(rev(models))
}

timing.strategy.test <- function() 
{
  models = timing.strategy(
    tickers = 'VTI,EFA,IEF,ICF,DBC,SHY',
    dates='2002:08::'
  )
  
  
  plotbt.custom.report.part1(models)
  
  plotbt.custom.report.part2(models)
  
  
}