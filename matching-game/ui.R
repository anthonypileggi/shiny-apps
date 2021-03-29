shinyUI(fluidPage(
  
  useShinyjs(),
  
  titlePanel("Matching Game"),
  
  sidebarLayout(
    
    sidebarPanel(
      actionButton("new_game", "Start a New Game"),
      hr(),
      uiOutput("info")
    ),

    mainPanel(
      plotOutput("gameboard", 
        click = "game_click", 
        hover = hoverOpts(id = "game_hover", delay = 100, delayType = c("throttle"))
        )
    )
    
  )
  
))
