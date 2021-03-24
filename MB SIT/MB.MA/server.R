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
    
    for(symbol in c(getStock(),getCash(),getSignal()) ) { #getStock, getCash, getSignal pull the tickers
      if (is.null(symbol_env[[symbol]]))
        tryCatch({
          symbol_env[[symbol]] = getSymbols(symbol, from = '1970-01-01', src='yahoo', auto.assign = FALSE)
        }, error = function(e) { stop(paste('Problem getting prices for',symbol,'make sure to separate with a comma and no spaces.')) })
      data[[symbol]] = adjustOHLC(symbol_env[[symbol]], use.Adjusted=T)  #adjusts all data for splits, etcs
    }
    
    #leverage stock and cash / if symbol is shared w/ signal, will leverage that unfortunately; could possibly fix by duplicating, consider later
    for(symbol in c(getStock(),getCash()) ) {
        tryCatch({
          data[[symbol]] =  create.leveraged(data[[symbol]], getLeverage())
        }, error = function(e) { stop(paste('Problem leveraging',symbol)) })
    }
    
    # limits to dates specified in UI
    bt.prep(data, align='remove.na', dates = paste(c(getStartdate(), getEnddate()), collapse="::"))
    data		
  })
  
  # Helper fns / might have more than i need, but started will all of them to avoid errors
  getSignal <- reactive({ toupper(input$signalticker) })
  getStock <- reactive({ toupper(input$stock) })
  getCash <- reactive({ toupper(input$cash) })
  
  getLeverage <- reactive({(input$n.leverage)})

  getStartdate <- reactive({ (input$start.date) }) 
  getEnddate <- reactive({ (input$end.date) })
  
  getSMAPeriod <- reactive({ (input$smaLen) })
  getProportion <- reactive({ (input$proportion) })
  getBand <- reactive({ (input$band) })

  getBackTest <- reactive({ 
    
    #*****************************************************************
    # Load historical data
    #******************************************************************  
    data = getData()
    
    tryCatch({		
      prices = data$prices
      n = ncol(prices)
      
      ###various variables
      signalticker = getSignal()
      stock = getStock()
      cash = getCash()
      
      SMAPeriod = getSMAPeriod() #SMA day length
      Proportion = (as.numeric(as.character(getProportion()))/100) #proportion of stock-to-cash in strategy 2 mix
      BandPer = (as.numeric(as.character(getBand()))/100) #convert band size to %
      BandPerUp = 1+BandPer
      BandPerDn = 1-BandPer

      period.ends = endpoints(prices, input$rebalance.freq) #rebelance/trade frequency
      period.ends = period.ends[period.ends > 0]
      
    
      models = list()
      
      #*****************************************************************
      # Stock
      #******************************************************************
      data$weight[] = NA
      data$weight[period.ends,stock] = 1
      models$stock = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
      
      #*****************************************************************
      # Stock/Cash Proportion
      #******************************************************************
      data$weight[] = NA
      data$weight[period.ends,stock] = (Proportion) * ntop(prices[period.ends,stock], n) 
      data$weight[period.ends,cash] = (1-Proportion) * ntop(prices[period.ends,cash], n)
      models$stock.cash.mix = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
      
      
      #*****************************************************************
      # Stock + 10 month go to cash filter
      #******************************************************************
      sma = bt.apply.matrix(prices, SMA, as.numeric(SMAPeriod))
      
      data$weight[] = NA
      data$weight[period.ends,stock] = iif(prices[period.ends,signalticker] > sma[period.ends,signalticker], 1, 0)
      data$weight[period.ends,cash] = 1 - ifna( ifna.prev(data$weight[period.ends,stock]), 0)
      models$stock.cash.MAfilter = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
      
      #*****************************************************************
      # Stock + 10 month +5/-5% go to cash filter
      #******************************************************************
      data$weight[] = NA
      
      data$weight[period.ends,stock] = iif(
                        prices[period.ends,signalticker] > (sma[period.ends,signalticker]*BandPerUp), #condition      
                        1,  #true
                        iif( #false:fx
                            prices[,signalticker] < (sma[period.ends,signalticker]*BandPerDn), #false:condition
                            0, #false:true 
                            NA) #false:false
                          )#close original function
       data$weight[period.ends,cash] = 1 - ifna( ifna.prev(data$weight[period.ends,stock]), 0)
       models$stock.cash.MA.Band = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
      
      
 #######################
#two additional strategies suggested by The Systematic Investor that didn't produce good results
       #not yet rewritten to take input from UI, just original R code
 ########################      
       #*****************************************************************
       # stock + dynamic cash filter base on volatility
       #******************************************************************
       #ret = diff(log(prices))
       #hist.vol = bt.apply.matrix(ret, runSD, n = 21)
       #
       #vol.rank = bt.apply.matrix(hist.vol, percent.rank, 252)
       #
       #sma.cash = sma * NA
       #sma.cash[] = iif(vol.rank < 0.5, bt.apply.matrix(prices, SMA, 10*22), bt.apply.matrix(prices, SMA, 1*22))
       #
       #data$weight[] = NA
       #data$weight$stock = iif(prices$stock <= sma$stock | prices$stock <= sma.cash$stock, 0, iif(prices$stock > sma$stock, 1, NA))
       #data$weight$cash = 1 - ifna( ifna.prev(data$weight$stock), 0)
       #models$stock.cash.VOL.SIMPLE = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
       
       #*****************************************************************
       # Stock + dynamic cash  filter base on volatility; multiple levels
       #******************************************************************
       #nbreaks = 5
       #map.index = seq(0,1, 1/nbreaks)
       #map = bt.apply.matrix(vol.rank, function(x) as.numeric(cut(x, map.index)))
       
       #sma.cash = sma * NA
       #for(i in 1:nbreaks) {
       #  temp = coredata(bt.apply.matrix(prices, SMA, (nbreaks - i + 1)* 2 *22))
       #  index = ifna(map == i, F)
       #  sma.cash[index] = temp[index]
       #}
       
       #data$weight[] = NA
       #data$weight$stock = iif(prices$stock <= sma$stock | prices$stock <= sma.cash$stock, 0, iif(prices$stock > sma$stock, 1, NA))
       #data$weight$cash = 1 - ifna( ifna.prev(data$weight$stock), 0)
       #models$stock.cash.VOL = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
       
       
       #data$weight[] = NA
       #data$weight$stock = iif(prices$stock <= sma$stock | prices$stock <= sma.cash$stock, 0, iif(prices$stock > sma$stock, 1, NA))
       #data$weight$cash = 1 - ifna( ifna.prev(data$weight$stock), 0)
       #models$stock.cash.VOL = bt.run.share(data, clean.signal=T, trade.summary=T, silent=T)
       
       
      
      
      rev(models)
    }, error = function(e) { stop(paste('Problem running Back Test:', e)) })
  })     
  
  
  
  
  
  #############
  #Plots and Tables for UI / they're repeated again in another section/ could be cleaned up
  ############
  
  # Make table / compares the strategies, included engineering.return to include trade # and some other stats
  makeSidebysideTable <- reactive({
    models = getBackTest()
    plotbt.strategy.sidebyside(models, make.plot=T, return.table=T,perfromance.fn = engineering.returns.kpi)
  })
  
  # Make table / monthly table, currently specifies model 1, which is MA Band; 2 = MA
  makeAnnualTable <- reactive({
    models = getBackTest()
    plotbt.monthly.table(models[[1]]$equity, make.plot = F)
  })
  
  # Make table / trade table, defaults to MA Band; could specify through # e.g. models[[2]]] to switch
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
  

  #*****************************************************************
  #Outputting the plots/tables -- could be cleaned by combining w/ prior section
  #******************************************************************    	
  # Generate a plot
  output$strategyPlot <- renderPlot({
    models = getBackTest()
    
    plota.theme(col.x.highlight = col.add.alpha('green',50))
    plotbt.custom.report.part1(models)  					
  }, height = 400, width = 600)
  
  # Generate a table
  output$sidebysideTable <- reactive({
    temp = makeSidebysideTable()	
    tableColor(as.matrix(temp), include.rownames=TRUE,
               make.plot=T, return.table=T,
               perfromance.fn = engineering.returns.kpi
    )		
  })
  
  # Generate a table
  output$annualTable <- reactive({
    temp = makeAnnualTable()	
    tableColor(as.matrix(temp), include.rownames=TRUE)		
  })
  
  # Generate a plot
  output$transitionPlot <- renderPlot({
    models = getBackTest()
    plotbt.transition.map(models[[1]]$weight)	
  }, height = 400, width = 600)
  
  # Generate a table
       output$tradesTable <- reactive({
         temp = makeTradesTable()	
         tableColor(as.matrix(temp), include.rownames=TRUE)		
       })
  
  
  
  #*****************************************************************
  # Download
  #******************************************************************    
  # Download pdf report
  output$downloadReport <- downloadHandler(
    filename = 'report.pdf',
    content = function(file) {
      pdf(file = file, width=8.5, height=11)
      
      models = getBackTest()
      
      plota.theme(col.x.highlight = col.add.alpha('green',50)) #not sure if the green signal highlight is working
      
      ###transition table, monthly, pie chart, verbose summary, and trade table for model 1, MA Bands
      plotbt.custom.report(models, trade.summary = T, x.highlight = models$market.filter$highlight)

      ###trade table for model 2, MA
      plotbt.custom.report.part3(models[2], trade.summary=T)
      
      dev.off()
    }
  )	
  
  # Download csv data
  output$downloadData <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      cat('Summary Performance:\n', file=file, append=F)
      write.table(makeSidebysideTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      		
      
      cat('\n\nAnnual Perfromance:\n', file=file, append=T)
      write.table(makeAnnualTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      
      
      cat('\n\nLast 20 Trades:\n', file=file, append=T)
      write.table(makeTradesTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      
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
