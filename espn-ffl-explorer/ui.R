library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("ESPN Fantasy Football -- League Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("id", "League ID:", value=431416),
      actionButton("fetch", "Retrieve League Data", icon = icon("refresh"))
    ),
    
    mainPanel(
      h4(textOutput("leagueInfo")),
      tabsetPanel(
        tabPanel("Standings", DT::dataTableOutput('standings')), 
        tabPanel("Score Dist'n", plotlyOutput("plotScores", height="500px"))),
        tabPanel("Season Grid", plotlyOutput("plotMatchups", height="600px")),
        tabPanel("Power Rankings", DT::dataTableOutput('power_rankings'),
                                   div("If every team played every other team each week.")) 
      )
    )
  )
))
