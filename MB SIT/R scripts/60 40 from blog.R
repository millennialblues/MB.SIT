####New 60/40
#####http://systematicinvestor.github.io/strategy/Strategy-NEW-60-40
#####Following report is based on Monthly re-balancing, 
######signal is generated one day before the month end, 
#######and execution is done at close at the month end.

#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = '
STOCK = SPY + VTSMX + VFINX
BOND = TLT + VUSTX
CASH = SHY + TB3Y
'

# load saved Proxies Raw Data, data.proxy.raw
load('data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')

print(last(data$prices))

#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
# do not allocate to CASH
data$universe$CASH = NA 

prices = data$prices * data$universe
n = ncol(prices)


#*****************************************************************
# Traditional, Dollar Weighted 40% Bonds & 60% Stock
#******************************************************************
target.allocation = NA * prices[1,]
target.allocation$STOCK = 60/100
target.allocation$BOND = 40/100

obj$weights$dollar.w.60.40 = rep.row(target.allocation, len(period.ends))

#*****************************************************************
# Risk Weighted 40% Bonds & 60% Stock
#******************************************************************
ret = diff(log(prices))
hist.vol = bt.apply.matrix(ret, runSD, n = 20)

# risk-parity
weight.risk = 1 / hist.vol
weight.risk = weight.risk / rowSums(weight.risk, na.rm=T)

obj$weights$risk.w.60.40 = weight.risk[period.ends,]

#*****************************************************************
# Cash Filter
#******************************************************************
# compute 10 month moving average
sma = bt.apply.matrix(prices, SMA, 200)

# go to cash if prices falls below 10 month moving average
go2cash = prices < sma
go2cash = ifna(go2cash, T)[period.ends,]


weight = obj$weights$risk.w.60.40
weight[go2cash] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)
obj$weights$risk.w.60.40.CASH = weight


weight[] = obj$weights$dollar.w.60.40
weight[go2cash] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)
obj$weights$dollar.w.60.40.CASH = weight

#*****************************************************************
# Scale Risk Weighted 40% Bonds & 60% Stock strategy to have 6% volatility
#****************************************************************** 
models = get.back.test(data, obj, input)

weight = target.vol.strategy(models$risk.w.60.40, ifna(weight.risk,0),
                             target=6/100, lookback.len=21, max.portfolio.leverage=100/100)

# invested not allocated to CASH
weight$CASH = 1 - rowSums(weight)

obj$weights$risk.w.60.40.target6.cash = weight[period.ends,]


