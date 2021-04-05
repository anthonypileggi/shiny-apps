
shinyServer(function(input, output, session) {
   
  # Functions ----------------------------
  
  # start a new game
  start_new_game <- function() {
    
    print("Starting a new game...")
    
    # wipe away old game
    rv$click <- dplyr::filter(rv$click, id < 0)
    rv$flag <- dplyr::filter(rv$flag, id < 0)
    
    # randomly draw mine locations
    rv$mines <- tibble::tibble(
      id = sample(1:(input$board_size^2), size = input$mines, replace = F),
      mine = T
    )
    
    # construct game board
    rv$game <- draw_board(input$board_size, rv$mines) %>%
      compute_counts()
    
    # start playing
    rv$playing <- T
    rv$time_elapsed <- 0
    
    print(rv$game)
  }
  
  
  # Reactives ----------------------------
  
  rv <- 
    reactiveValues(
      wins = 0,
      playing = F,
      mines = tibble::tibble(id = integer(), mine = logical()),
      game = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      click = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      flag = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      hover = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      time_elapsed = 0
      )

  # Gameboard (reactive plot)
  gameboard <- reactive({
    rv$game %>%
      ggplot(aes(x = i, y = j)) +
      geom_tile(color = "black", fill = "blue", alpha = .2) +
      theme_void() +
      theme(
        aspect.ratio = 1
      )
  })
  
  
  # Observers --------------------------
  
  # start a new game
  observeEvent(input$new_game, {
    start_new_game()
  })
  
  # observe({
  #   invalidateLater(1000, session)
  #   rv$time_elapsed <- rv$time_elapsed + 1
  # })
  
  # update highlighted cell based on user cursor
  observeEvent(input$game_hover, {
    rv$hover <- rv$game %>%
      dplyr::filter(
        input$game_hover$x > i - .5,
        input$game_hover$x < i + .5,
        input$game_hover$y > j - .5,
        input$game_hover$y < j + .5
      )
  })
  
  # record user click + react
  observeEvent(input$game_click, {
    
    # record click location
    new_click <- rv$game %>%
      dplyr::filter(
        input$game_click$x > i - .5,
        input$game_click$x < i + .5,
        input$game_click$y > j - .5,
        input$game_click$y < j + .5
      )
    if (nrow(new_click) > 0 & rv$playing) {
      if (!is.element(new_click$id, rv$click$id))
        rv$click <- dplyr::bind_rows(rv$click, new_click)
    }
    print(rv$click)  

    # check if game is over 
    #  -- does rv$flags include all mines?  has user clicked every non-mine cell
    if (all(rv$mines$id %in% rv$flag$id)) {
      print("YOU WIN!")
      rv$wins <- rv$wins + 1
      rv$playing <- F
    }
      
    #  -- does rv$click include a mine?
    if (any(rv$click$mine)) {
      print("GAME OVER!")
      rv$playing <- F
    }
    
  })
  
  
  # Outputs -------------------------
  
  # plot gameboard
  output$gameboard <- renderPlot({
    g <- gameboard()
    if (rv$playing)
      g <- g + geom_tile(color = "black", fill = "blue", alpha = .25, data = rv$hover)
    g +
      geom_tile(color = "black", fill = "red", alpha = .75, data = dplyr::filter(rv$click, mine)) +
      geom_text(aes(label = count, color = count), size = 14, data = dplyr::filter(rv$click, !mine)) +
      scale_color_gradient2(low = "green", mid = "orange", high = "red", midpoint = 3, limits = c(0, 9), guide = F)
  })

  output$gameboard_placeholder <- renderUI({
    if (input$new_game == 0) {
      div(
        #tags$img(src = "https://media.giphy.com/media/l3V0GQMoaDLVbjXEI/giphy.gif", style = "width: 75%"),
        h2("Minesweeper")
      )
    }
  })
  
  # timer
  output$timer <- renderUI({
    invalidateLater(1000, session)
    #rv$time_elapsed <- rv$time_elapsed + 1
    paste("Time Elapsed: ", lubridate::seconds_to_period(rv$time_elapsed))
  })
  
  # game status report
  output$info <- renderUI({
    req(rv$game)
    if (input$new_game > 0) {
      div(
        h3("Profile"),
        h4(input$name),
        paste(rv$wins, "Wins"),
        hr(),
        h3("Game Status"),
        #paste("Time Elapsed: ", lubridate::seconds_to_period(rv$time_elapsed)),
        br(),
        paste(nrow(rv$click), " / ", sum(!rv$game$mine), "(", scales::percent(nrow(rv$click) / sum(!rv$game$mine)), ")"),
        if (!rv$playing) h4("GAME OVER", style = "color:red")
      )    
    }

  })
  
  
})
