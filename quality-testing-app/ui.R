library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Parts Testing -- Stopping Rules"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("items", "Batch size:", 400, min=1),
      numericInput("fail_cost", "Cost of selling a faulty unit ($):", 40, min=0),
      numericInput("labor_speed", "Testing Speed (units/hour): ", 100, min=0),
      numericInput("n", "Total units tested:", 0, min=0),
      numericInput("x", "Total units failed:", 0, min=0)
    ),
    
    mainPanel(
      h3(textOutput("textStopTime")),
      h4(textOutput("textStopProb")),
      plotlyOutput("ggPlot"),
      h4(textOutput("textMin")),
      h4(textOutput("textMax")),
      br(),
      h4(textOutput("textMean"))
    )
  )
))
