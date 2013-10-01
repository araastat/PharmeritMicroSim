library(shiny)
source('Sims1.R')

shinyServer(function(input,output){
  t1 <- system.time(out <- reactive({
    Nsim <- input$Nsim
    out <- colMeans(sims(Nsim,N))
  }))
  output$simplot <- renderPlot({
    hist(out(), xlab='Average number of handouts', main="")
  })
  output$summary <- renderPrint({
    summary(out())
  })
})