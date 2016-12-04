library(plotly)
library(shiny)

fluidPage( 
  

  
  titlePanel("Black-Scholes formula for European call price"),
  
  sidebarLayout(      
    
    sidebarPanel(
      
      numericInput("K", "Strike price:",100),
      sliderInput("r",
                  "Risk free interest rate:",
                  min = 0,  max = 1, value = 0.1,step = 0.1),
      numericInput("sigma", "Volatility:",0.3,step = 0.1),
    
      sliderInput("t",
                "Time t (t=c-τ):",
                min = 0,  max = 1, value = 0.1,animate=TRUE,step = 0.05),
      
      hr()
    ),

    
    
    mainPanel(
      
                tabPanel("Call", plotlyOutput("Plot_BS_Call"))
                  
    )
    
      
      
       
  )
  
  
  
  
  
)
