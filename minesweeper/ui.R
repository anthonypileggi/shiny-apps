shinyUI(fluidPage(
  
  useShinyjs(),
  
  titlePanel("Minesweeper"),
  
  sidebarLayout(
    
    sidebarPanel(
      #radioButtons("difficulty", "Difficulty", choices = c("easy", "medium", "hard"), inline = T),
      textInput("name", "What's Your Name?", placeholder = "Smarty Pants", value = ""),
      sliderInput("board_size", label = "Board Size", value = 10, min = 5, max = 20, step = 1),
      sliderInput("mines", label =  "Mines", value = 5, min = 1, max = 20, step = 1),
      actionButton("new_game", "Start a New Game"),
      hr(),
      uiOutput("info"),
      uiOutput("timer")
    ),

    mainPanel(
      uiOutput("gameboard_placeholder"),
      plotOutput(
        "gameboard", 
        click = "game_click", 
        hover = hoverOpts(id = "game_hover", delay = 100, delayType = c("throttle"))
      )
    )
    
  )
  
))
