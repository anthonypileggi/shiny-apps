
shinyServer(function(input, output) {
   
  rv <- 
    reactiveValues(
      wins = 0,
      best = 50,
      turn = 0,
      matches = 0,
      click = tibble::tibble(i = integer(), j = integer(), id = integer(), item = character()),
      hover = tibble::tibble(i = integer(), j = integer(), id = integer(), item = character()),
      match = tibble::tibble(i = integer(), j = integer(), id = integer(), item = character())
      )
  
  # start a new game
  observeEvent(input$new_game, {
    rv$match <- dplyr::filter(rv$match, id < 0)
    rv$click <- dplyr::filter(rv$click, id < 0)
    rv$turn <- 0
    rv$matches <- 0
  })
  
  # input list of matching items
  items <- reactive({
    letters[1:8]
  })
  
  # card locations
  cards <- reactive({
    tibble::tibble(
      id = 1:16,
      item = sample(rep(items(), 2))
    )
  })
  
  # game (as data.frame)
  game_df <- reactive({
    expand.grid(i = 1:4, j = 1:4) %>% 
      mutate(id = row_number()) %>%
      left_join(cards(), by = "id")
  })
  
  # game (as matrix)
  game_mat <- reactive({
    x <- matrix(NA, nrow = 4, ncol = 4)
    x[cards()$id] <- cards()$item
    x
  })
  
  gameboard <- reactive({
    game_df() %>%
      ggplot(aes(x = i, y = j)) +
      geom_tile(color = "black", fill = "blue", alpha = .2) +
      theme_void()
  })
  
  # update selection based on user click
  observeEvent(input$game_click, {
    new_click <- game_df() %>%
      dplyr::filter(
        input$game_click$x > i - .5,
        input$game_click$x < i + .5,
        input$game_click$y > j - .5,
        input$game_click$y < j + .5
      )
    if (nrow(new_click) > 0 & nrow(rv$click) < 2) {
      if (!is.element(new_click$id, rv$click$id))
        rv$click <- dplyr::bind_rows(rv$click, new_click)
    } 
      
  })
  
  # check if it's a matching pair, and start a new turn
  observeEvent(input$game_click, {
    if (nrow(rv$click) == 2) {
      if (rv$click$item[1] == rv$click$item[2]) {              # check if they are a match
        rv$match <- dplyr::bind_rows(rv$match, rv$click)       # save matches
        rv$matches <- rv$matches + 1
      }             
      delay(500, rv$click <- dplyr::filter(rv$click, id < 0)) # remove all clicks
      rv$turn <- rv$turn + 1
    }
    # check if the game is over, and update player's record
    if (rv$matches == 8) {
      rv$wins <- rv$wins + 1
      rv$best <- min(c(rv$best, rv$turn))
    }
  })
  
  # update highlighted cell based on user cursor
  observeEvent(input$game_hover, {
    rv$hover <- game_df() %>%
      dplyr::filter(
        input$game_hover$x > i - .5,
        input$game_hover$x < i + .5,
        input$game_hover$y > j - .5,
        input$game_hover$y < j + .5
      )
  })
  
  # plot gameboard
  output$gameboard <- renderPlot({
    gameboard() +
      geom_tile(color = "black", fill = "blue", alpha = .5, data = rv$hover) +
      geom_text(aes(label = item), size = 14, data = rv$click) +
      geom_text(aes(label = item), size = 14, color = "red", data = rv$match)
  })
  

  # game status report
  output$info <- renderUI({
    div(
      h3("Record"),
      paste(rv$wins, "Wins"),
      br(),
      paste("Best game completed in ", rv$best, "turns."),
      hr(),
      h3("Current Turn"),
      paste(rv$turn),
      hr(),
      h3("Game Status"),
      paste("Found", rv$matches, "out of 8 pairs."),
      hr(),
      h3("What Next"),
      dplyr::case_when(
        rv$matches == 8 ~ "You did it!  Start a new game.",
        nrow(rv$click) == 0 ~ "Make your first selection.",
        nrow(rv$click) == 1 ~ "Make your second selection.",
        nrow(rv$click) == 2 ~ "Do they match?"
      )
    )
  })
  
})
