
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
  

  
 
  input_values <- reactive({
    
    res<-rep(0,5)
    
    res[1]<-input$K
    res[2]<-input$r
    res[3]<-input$t
    res[4]<-input$sigma
    
    res
    
    
  })
  
  ##  Graph  ##
  
  output$Plot_BS_Call <- renderPlotly({
    
    data <-input_values()
    
    x<-c(0:200)
    
    p <- plot_ly(x =~x,y=seq(0,120),name = "",line = list(shape = "linear",color = 'rgb(252, 252, 252)')) %>%
      
      add_trace(y = bs_call(x, data[1], data[2],1- data[3], data[4]), name = "BSE call price", line = list(shape = "linear",color = 'rgb(252, 0, 0)')) %>%
      
      
      
      add_trace(y = pmax(0,x-data[1]), name = "Payoff at maturity", line = list(shape = "linear",color = 'rgb(0, 0, 252)')) %>%
    
      layout(yaxis = list(title = 'C(S,τ)'),xaxis = list (title = 'S'))
    
    
  })
  
  
  


  
  
  
  
  
}
