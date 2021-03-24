###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
load(SIT)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod,quadprog,corpcor,lpSolve')
tickers = spl('TNA,TQQQ,TMF,CURE,LABU,SOXL,DPST,DRN')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
bt.prep(data, align='remove.na', dates='1990::') 

#*****************************************************************
# Code Strategies
#******************************************************************                     
cluster.group = cluster.group.kmeans.90

obj = portfolio.allocation.helper(data$prices, 
                                  periodicity = 'months', lookback.len = 60, 
                                  min.risk.fns = list(
                                    C.EW.kmeans = distribute.weights(equal.weight.portfolio, cluster.group.kmeans.90),
                                    C.EW.FTCA = distribute.weights(equal.weight.portfolio, cluster.group.FTCA(0.5)),
                                    
                                    C.RP.kmeans = distribute.weights(risk.parity.portfolio(), cluster.group.kmeans.90),
                                    C.RP.FTCA = distribute.weights(risk.parity.portfolio(), cluster.group.FTCA(0.5)),
                                    
                                    C.MD.kmeans = distribute.weights(max.div.portfolio, cluster.group.kmeans.90),
                                    C.MD.FTCA = distribute.weights(max.div.portfolio, cluster.group.FTCA(0.5)),
                                    
                                    C.MV.kmeans = distribute.weights(min.var.portfolio, cluster.group.kmeans.90),
                                    C.MV.FTCA = distribute.weights(min.var.portfolio, cluster.group.FTCA(0.5)),
                                    
                                    C.MVE.kmeans = distribute.weights(min.var.excel.portfolio, cluster.group.kmeans.90),
                                    C.MVE.FTCA = distribute.weights(min.var.excel.portfolio, cluster.group.FTCA(0.5)),
                                    
                                    C.MCE.kmeans = distribute.weights(min.corr.excel.portfolio, cluster.group.kmeans.90),
                                    C.MCE.FTCA = distribute.weights(min.corr.excel.portfolio, cluster.group.FTCA(0.5)),
                                    
                                    C.MS.kmeans = distribute.weights(max.sharpe.portfolio(), cluster.group.kmeans.90),
                                    C.MS.FTCA = distribute.weights(max.sharpe.portfolio(), cluster.group.FTCA(0.5)),
                                    
                                    C.ERC.kmeans = distribute.weights(equal.risk.contribution.portfolio, cluster.group.kmeans.90),
                                    C.ERC.FTCA = distribute.weights(equal.risk.contribution.portfolio, cluster.group.FTCA(0.5))
                                  )
)

models = create.strategies(obj, data)$models

#*****************************************************************
# Create Report
#******************************************************************    
strategy.performance.snapshoot(models, T, 'Backtesting Asset Allocation portfolios')