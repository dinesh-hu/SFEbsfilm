library(plotly)
library(shiny)


function(input, output) {
  
  ##  Black-Scholes formula for European call ##
  
  bs_call <-function(S, K, r, tau, sigma) {
    # Black-Scholes formula for European call price with b = r (costs of carry =
    # risk free interest rate -> the underlying pays no continuous dividend)
    d1 = (log(S/K) + (r + (sigma^2/2)) * tau)/(sigma * sqrt(tau))
    d2 = d1 - (sigma * sqrt(tau))
    C  = S * pnorm(d1) - K * exp(-r * tau) * pnorm(d2)
    return(C)
  }
  
  values <- reactive({
    
    res<-rep(0,4)
    
    res[1]<-input$K
    res[2]<-input$r
    res[3]<-input$tau
    res[4]<-input$sigma
    
    res
    
    
  })
  
  
  output$Plot_BS <- renderPlotly({
    
    donnee <-values()
    
    x <- c(0:170)
    
    p <- plot_ly(x = ~x) %>%
      add_trace(y = bs_call(x, donnee[1], donnee[2],1- donnee[3], donnee[4]), name = "t1", line = list(shape = "linear")) %>%
      layout(yaxis = list(title = 'C(S,τ)'),xaxis = list (title = 'S'))
    
    
  })
  
  
  
}