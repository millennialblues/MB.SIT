
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  headerPanel(""),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(   
    selectInput(inputId =  "start.date", 
                label = "Start:", 
                choices = 1970:as.numeric(format(Sys.Date(),"%Y")),
                selected = 1970
    ),
    selectInput(inputId =  "end.date", 
                label = "End:", 
                choices = 1970:as.numeric(format(Sys.Date(),"%Y")),
                selected = as.numeric(format(Sys.Date(),"%Y"))
    ),
    textInput("signalticker", "Signal:", value = "^GSPC"), 
    textInput("stock", "Stock:", value = "VFINX"), 
    textInput("cash", "Cash:", value = "PINCX"), 
    numericInput("n.leverage", "Leverage (if stock or cash are used as signal, it will inadvertently get leveraged):", 3),
    numericInput("proportion", "Percentage of stock-to-cash in stock/cash mix:", value=55),
    numericInput("smaLen", strong("Moving average in days:"), 180),
    selectInput(inputId =  "rebalance.freq", 
                label = "Trading frequency:", 
                list("Yearly" = "years", "Quarterly" = "quarters", "Monthly" = "months", "Daily" = "days"),
                selected = "days"
    ),
    numericInput("band", "Percentage band above/below MA:", 3),
    numericInput("n.tradeview", "Number of trades to view:", 40),
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
               h4("Last Band Trades"),
               tableOutput("tradesTable"),				
               downloadButton("downloadReport", "Download Report"),
               downloadButton("downloadData", "Download Data"),
               br(),
               br()	
  )
))

