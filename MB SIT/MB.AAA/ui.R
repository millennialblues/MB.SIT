
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

	headerPanel(""),

	# Sidebar with a slider input for number of observations
	sidebarPanel(
	  selectInput(inputId =  "start.date", 
	    label = "Start:", 
	   choices = 1980:as.numeric(format(Sys.Date(),"%Y")),
	   selected = 2000
	   ),
	 	 selectInput(inputId =  "end.date", 
	              label = "End:", 
	             choices = 1980:as.numeric(format(Sys.Date(),"%Y")),
	              selected = as.numeric(format(Sys.Date(),"%Y"))
	              ),
	  tags$label("Yahoo Tickers separated by commas with no spaces:"),
		tags$textarea(id = "symbols", "SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD"),
		#createNonReactiveTextInputCustom("symbols", "Yahoo Ticker(s) separated by comma or new line:", "textarea", "Update", enableEnter=F, opts=list(rows=10, cols=10, "XLY,XLP,XLE,XLF\nXLV,XLI,XLB,XLK\nXLU")),
		br(),
		numericInput("n.leverage", "Leverage:", 1),
		selectInput(inputId =  "rebalance.freq", 
		            label = "Rebalancing frequency:", 
		            list("Yearly" = "years", "Quarterly" = "quarters", "Monthly" = "months", "Daily" = "days"),
		            selected = "months"
		),
		selectInput("n.top", "Number of momentum positions:", choices =  1:12,selected=4),
		numericInput("n.mom", "Length of momentum lookback in months:", 5),
		numericInput("n.vol", "Length of volatility in days:", 11),				
		numericInput("n.tradeview", "Number of trades to view:", 12),
		br(),
		submitButton("Run"),
		htmlOutput("status")
	),

 
	# Show a plot of the generated distribution
	mainPanel( 
			  plotOutput("strategyPlot"),
				br(),
				tableOutput("sidebysideTable"),
				h4("Annual Perfromance"),
				tableOutput("annualTable"),
				h4("Transition Map"),
				plotOutput("transitionPlot"),
				h4("Last AAA Trades"),
				tableOutput("tradesTable"),
				downloadButton("downloadReport", "Download Report"),
				downloadButton("downloadData", "Download Data"),
				br(),
				br()	
			)			
        
))