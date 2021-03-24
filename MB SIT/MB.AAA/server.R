# Check for package / other dependencies include shiny, zoo, xts, quantmod, TTR, SIT.Date
require("kernlab")
require("SIT")

# Define server
shinyServer(function(input, output) {

	# Create an environment for storing data
	symbol_env <- new.env()
	
    #*****************************************************************
    # Shared Reactive functions
  	# http://rstudio.github.com/shiny/tutorial/#inputs-and-outputs
    #******************************************************************    	
    # Get stock data
  	getData <- reactive({  	
  		cat('getData was called\n')
  	
  		data <- new.env()
  		for(symbol in getStocks() ) { #getStock = pull the tickers for those symbols
  			if (is.null(symbol_env[[symbol]]))
  			  tryCatch({
				symbol_env[[symbol]] = getSymbols(symbol, from = '1970-01-01', src='yahoo', auto.assign = FALSE)
			}, error = function(e) { stop(paste('Problem getting prices for',symbol,'make sure to separate with a comma and no spaces.')) })
  			data[[symbol]] = adjustOHLC(symbol_env[[symbol]], use.Adjusted=T)  	#adjusts for splits, etc		
  		}
  	
  		#levers up
  		for(i in ls(data)) data[[i]] = create.leveraged(data[[i]], input$n.leverage)
  			
  		#limits dates to time-window specified in UI
  	  bt.prep(data, align='keep.all', dates = paste(c(getStartdate(), getEnddate()), collapse="::"))
		data		
	})
  	
	# Helper fns
	getStocks <- reactive({ spl(toupper(gsub('\n',',',input$symbols))) })
	getStartdate <- reactive({ (input$start.date) }) 
	getEnddate <- reactive({ (input$end.date) }) 
	
	getBackTest <- reactive({ 
		#*****************************************************************
		# Load historical data
		#******************************************************************  
		data = getData()

	tryCatch({		
		#*****************************************************************
		# Code Strategies / from AAA
		#****************************************************************** 
		prices = data$prices   
			nperiods = nrow(prices)
			n = ncol(prices)
		
		# find period ends / rebalancing frequency
		period.ends = endpoints(prices, input$rebalance.freq)
		period.ends = period.ends[period.ends > 0]
	 		
		models = list()
		
		# Adaptive Asset Allocation parameters
		n.top = as.numeric(input$n.top)       # number of momentum positions
		n.mom = as.numeric(input$n.mom) * 20    # length of momentum look back, turn into months
		n.vol = as.numeric(input$n.vol)    # length of volatility look back
		
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
	
		
  #		
		rev(models)
	}, error = function(e) { stop(paste('Problem running Back Test:', e)) })
	})
	
		
	# Make table
	makeSidebysideTable <- reactive({
		models = getBackTest()
		plotbt.strategy.sidebyside(models, return.table=T, make.plot=F)
})

	
		
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	# Make table
	makeTradesTable <- function(i = 1) {
		models = getBackTest()
		model = models[[i]]
		
	# View Parameter from UI
		n.tradeview = as.numeric(input$n.tradeview)        # number of trades to view
		
		
			if (!is.null(model$trade.summary)) {
			ntrades = min(n.tradeview, nrow(model$trade.summary$trades))		
			last(model$trade.summary$trades, ntrades)
		}
	}
	
	
	
	# Make table
	makeAnnualTable <- function(i = 1) {
		models = getBackTest()
		plotbt.monthly.table(models[[i]]$equity, make.plot = F)
	}
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	
		# Generate a plot
	output$strategyPlot <- renderPlot({
		models = getBackTest()		
		plotbt.custom.report.part1(models)  					
	}, height = 400, width = 600)

	# Generate a table
  	output$sidebysideTable <- reactive({
    temp = makeSidebysideTable()	
    tableColor(temp, include.rownames=TRUE)	
  	})
    
	# Generate a table
  	output$annualTable <- reactive({
		tableColor(as.matrix(makeAnnualTable(1)), include.rownames=TRUE)
	})

	# Generate a plot
	output$transitionPlot <- renderPlot({
		models = getBackTest()
		plotbt.transition.map(models[[1]]$weight)
	}, height = 400, width = 600)

	# Generate a table
  	output$tradesTable <- reactive({
  		tableColor(makeTradesTable(1), include.rownames=TRUE)
  	})
  	  

	
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		models <<- getBackTest()
    		
    		plotbt.custom.report.part1(models)

    		
    		plotbt.custom.report.part2(models[1])

    		plotbt.custom.report.part3(models[1], trade.summary=T)

    		
    		plotbt.custom.report.part2(models[2])

    		plotbt.custom.report.part3(models[2], trade.summary=T)

    		
			# Plot Portfolio Turnover for each strategy
			data = getData()
			layout(1)
			barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
      		
		    dev.off()
    	}
	)	
		
	# Download csv data
	output$downloadData <- downloadHandler(
    	filename = 'data.csv',
    	content = function(file) {
    		models = getBackTest()
    	
    		cat('Summary Performance:\n', file=file, append=F)
      		write.table(makeSidebysideTable(), sep=',',  col.names=NA, quote=F, file=file, append=T)      		
      		
      	for(i in 1:2) {
      		cat('\n\n', names(models)[i] ,'Annual Perfromance:\n', file=file, append=T)
      		write.table(makeAnnualTable(i), sep=',', col.names=NA, quote=F, file=file, append=T)      

      		cat('\n\n', names(models)[i] ,'Last AAA Trades:\n', file=file, append=T)
      		write.table(makeTradesTable(i), sep=',', col.names=NA, quote=F, file=file, append=T) 
      	
      	}
    	}
  	)	

    		  	
    #*****************************************************************
    # Update status message 
    #******************************************************************    
	output$status <- renderUI({
		out = tryCatch( getData(), error=function( err ) paste(err))	    				
		if( is.character( out ) ) 
			HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",out))
		else
			HTML("<b>Status</b>: <b><font color='green'>Ok</font></b>")		
})
	
})

