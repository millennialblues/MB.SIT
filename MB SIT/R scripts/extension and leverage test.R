#####extension test
#### nothing is working here


#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = spl('TMF,TLT,VUSTX')
#TLT.VUSTX=TLT+VUSTX

data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   





#create tlt2
#TLTx2 = create.leveraged(data$TLT, leverage=2)    

#visualize 
#proxy.test(list(UBT=data$UBT, TLTx2=TLTx2),price.fn=Ad)

#extend
#UBT = extend.data(data$UBT, create.leveraged(data$TLT, leverage=2), scale=T)
#TMF = extend.data(data$TMF, create.leveraged(data$TLT, leverage=3), scale=T)    

#VUSTX = TLT extension

#visualize
#proxy.test(list(TLT=data$TLT, UBT=UBT, TMF=TMF),price.fn=Ad)