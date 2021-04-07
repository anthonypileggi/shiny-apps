shinyUI(fluidPage(
  
  useShinyjs(),
  
  titlePanel("Minesweeper"),
  
  sidebarLayout(
    
    sidebarPanel(
      #radioButtons("difficulty", "Difficulty", choices = c("easy", "medium", "hard"), inline = T),
      #textInput("name", "What's Your Name?", placeholder = "Smarty Pants", value = ""),
      selectizeInput(
        "name", 
        label = "Choose your character",
        choices = c("dog" = "dog.png", "frog" = "frog.png"),
        selected = "dog",
        options = list(
          render = I(
            "{
          option: function(item, escape) {
          return '<div><img src=\"' + item.value + '\" width = 50 /> -- ' + escape(item.label) + '</div>'
            }
            }")
        )
      ),
      sliderInput("board_size", label = "Board Size", value = 10, min = 5, max = 20, step = 1),
      sliderInput("mines", label =  "Mines", value = 5, min = 1, max = 20, step = 1),
      actionButton("new_game", "Start a New Game"),
      hr()
    ),

    mainPanel(
      fluidRow(
        column(6, uiOutput("profile")),
        column(6, uiOutput("status"), uiOutput("timer")),
        column(
          width = 12,
          uiOutput("gameboard_placeholder"),
          plotOutput(
            "gameboard", 
            click = "game_click", 
            hover = hoverOpts(id = "game_hover", delay = 100, delayType = c("throttle"))
          )
        ),
        hr(),
        #h3("Gameplay Statistics"),
        column(6,  DT::dataTableOutput("history")),
        column(6, plotOutput("player_plot"))
      )
      
    )
    
  )
  
))
