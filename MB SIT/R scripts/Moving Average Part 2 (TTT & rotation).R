tickers = '
US.STOCKS = VTI + VTSMX
FOREIGN.STOCKS = VEU + FDIVX
US.10YR.GOV.BOND = IEF + VFITX
REAL.ESTATE = VNQ + VGSIX
COMMODITIES = DBC + CRB
CASH = BND + VBMFX,
SP500 = SPY
'

# load saved Proxies Raw Data, data.proxy.raw
load('data/data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')


#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
# do not allocate to CASH
data$universe$CASH = NA 
data$universe$SP500 = NA

prices = data$prices * data$universe
n = ncol(prices)

period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

models = list()

#*****************************************************************
# Benchmarks
#*****************************************************************
data$weight[] = NA
data$weight$SP500 = 1
models$SP500 = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)

data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$EW = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

#*****************************************************************
#The [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)
#[SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461)
#*****************************************************************
sma = bt.apply.matrix(prices, SMA, 10*22)

weight = NA * data$weight

weight = iif(prices > sma, 20/100, 0)
weight$CASH = 1 - rowSums(weight)

data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$Model = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

#*****************************************************************
# Alternative: MA bands
#*****************************************************************
sma = bt.apply.matrix(prices, SMA, 10*22)
signal = iif(cross.up(prices, sma * 1.05), 1, iif(cross.dn(prices, sma * 0.95), 0, NA))
signal = ifna(bt.apply.matrix(signal, ifna.prev),0)

weight = iif(signal == 1, 20/100, 0)
weight$CASH = 1 - rowSums(weight)

data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$Model.B = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T)

#*****************************************************************
# Report
#*****************************************************************
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)