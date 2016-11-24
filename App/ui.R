library(plotly)
library(shiny)


fluidPage(    
  
  # Give the page a title
  titlePanel("Black–Scholes Formula for European Options"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      numericInput("S", "Stock price:",110),
      numericInput("K", "Strike price:",100),
      sliderInput("r",
                  "Risk free interest rate:",
                  min = 0,  max = 1, value = 0.1,step = 0.1),
      numericInput("sigma", "Volatility:",0.3),
    
      sliderInput("tau",
                "Time :",
                min = 0,  max = 1, value = 0.1,animate=TRUE,step = 0.1)
    ),

    
    mainPanel(
      
      plotlyOutput("Plot_BS")
       
    )
    
  )

  ###  
)



