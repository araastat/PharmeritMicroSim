library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Microsimulation of ISPOR"),
  sidebarPanel(
    numericInput("Nsim","Number of simulations:",50),
    submitButton("Run")
  ),
  
  mainPanel(
    plotOutput("simplot"),
    verbatimTextOutput("summary")
  )
))