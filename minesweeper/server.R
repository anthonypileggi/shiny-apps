
shinyServer(function(input, output, session) {
   
  # Functions ----------------------------
  
  # start a new game
  start_new_game <- function() {
    
    print("Starting a new game...")
    
    # wipe away old game
    rv$click <- dplyr::filter(rv$click, id < 0)
    rv$flag <- dplyr::filter(rv$flag, id < 0)
    
    # construct game board
    rv$game <- draw_board(input$board_size, input$mines) %>%
      compute_counts()
    
    # extract mines
    rv$mines <- dplyr::filter(rv$game, mine)
    
    # start playing
    rv$playing <- T
    rv$start_time <- Sys.time()
    rv$time_elapsed <- 0
    
    print(rv)
  }
  
  # end current game
  end_game <- function(result = c("win", "loss")) {
    
    result <- match.arg(result)
    
    # lock gameboard
    rv$playing <- F
    
    # alert user of game outcome; increment
    rv$played <- rv$played + 1
    if (result == "win") {
      rv$wins <- rv$wins + 1
      sendSweetAlert(
        session = session,
        title = "YOU WIN!",
        type = "success"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "YOU LOSE!",
        type = "error"
      )
    }
    
    # record results to logs
    out <- dplyr::tibble(
      player = input$name,
      country = NA,       # get user location automatically from session-info
      size = input$board_size,
      mines = input$mines,
      result = result,
      revealed = sum(!rv$click$mine),
      total = sum(!rv$game$mine),
      score = revealed / total,     # percent complete ???
      time_elapsed = as.numeric(round(difftime(rv$current_time, rv$start_time, units = "secs"))),
      date = Sys.time()
    )

    bq_append(out, dataset = "minesweeper", table = "history")
      
  }
  
  
  # Reactives ----------------------------
  
  rv <- 
    reactiveValues(
      played = 0,
      wins = 0,
      playing = F,
      mines = tibble::tibble(id = integer(), mine = logical()),
      game = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      click = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      flag = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      hover = tibble::tibble(i = integer(), j = integer(), id = integer(), mine = logical(), count = double()),
      start_time = NULL,
      current_time = NULL,
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
  
  # History
  history <- reactive({
    bq_get(dataset = "minesweeper", table = "history")
  })
  
  # Observers --------------------------
  
  # start a new game
  observeEvent(input$new_game, {
    start_new_game()
  })

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
      if (!is.element(new_click$id, rv$click$id)) {
        print("click...")
        print(new_click)
        print("board")
        print(rv$game)
        new_click <- expand_click(rv$game, new_click)
         # dplyr::filter(id %in% rv$click)  # ignore already clicked cells
        print(new_click)
        rv$click <- dplyr::bind_rows(rv$click, new_click)
      }
    }
    print(rv$click)  

    # check if game is over 
    if (rv$playing) {
      
      #  -- does rv$flags include all mines? Or, has user clicked every non-mine cell
      if (all(rv$mines$id %in% rv$flag$id) | sum(!rv$click$mine) == sum(!rv$game$mine))
        end_game("win")
      
      #  -- does rv$click include a mine?
      if (any(rv$click$mine))
        end_game("loss")
      
    }
    
    
  })
  
  
  # Outputs -------------------------
  
  # plot gameboard
  output$gameboard <- renderPlot({
    g <- gameboard()
    if (rv$playing)
      g <- g + geom_tile(color = "black", fill = "blue", alpha = .5, data = rv$hover)
    if (!rv$playing)
      g <- g + geom_point(color = "red", fill = "red", alpha = .85, size = 3, data = dplyr::filter(rv$game, mine))
    
    g +
      geom_tile(color = "black", fill = "red", alpha = .65, data = dplyr::filter(rv$click, mine)) +
      geom_tile(color = "black", fill = "blue", alpha = .25, data = dplyr::filter(rv$click, !mine)) +
      geom_text(aes(label = count, color = count), size = 14, data = dplyr::filter(rv$click, !mine, count != 0)) +
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
  
  # game timer  
  output$timer <- renderUI({
    if (rv$playing) {
      invalidateLater(1000, session)
      rv$current_time <- Sys.time()
      tmp <- as.numeric(round(difftime(rv$current_time, rv$start_time, units = "secs")))
      paste("Time Elapsed: ", lubridate::seconds_to_period(tmp))
    }
  })
  
  # player profile
  output$profile <- renderUI({
    req(rv$game)
    if (input$new_game > 0) {
      div(
        h3("Profile"),
        #h4(input$name),
        tags$img(src = input$name),
        br(),
        h4(paste(rv$wins, "Wins  --  ", rv$played - rv$wins, "Losses"))
      )
    }
  })
  
  # game status
  output$status <- renderUI({
    req(rv$game)
    if (input$new_game > 0) {
      div(
        h3("Game Status"),
        br(),
        h4(
          paste(
            nrow(rv$click), " / ", sum(!rv$game$mine), 
            "(", scales::percent(nrow(rv$click) / sum(!rv$game$mine)), ")"
          )
        ),
        if (!rv$playing) 
          h4("~~ GAME OVER ~~", style = "color:red")
      )    
    }
  })
  
  # last 100 games
  output$history <- DT::renderDataTable({
    req(history())
    history() %>%
      tail(100) %>%
      dplyr::select(player, size, mines, result, score, time = time_elapsed) %>%
      dplyr::mutate(
        player = purrr::map_chr(player, ~as.character(tags$img(src = .x, style = "height:30px")))
      ) %>%
      DT::datatable(
        caption = "Gameplay History",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(
          dom = "tp",
          pageLength = 5,
          scrollX = T
        ) 
      ) %>%
      DT::formatPercentage("score", digits = 1)
  })
  
  # player vs. player history (bar graph)
  output$player_plot <- renderPlot({
    req(history())
    history() %>% 
      dplyr::mutate(
        url = paste0("www/", player),
        player = purrr::map_chr(url, ~as.character(tags$img(src = .x)))
      ) %>% 
      dplyr::group_by(url, player) %>% 
      dplyr::summarize(
        games = dplyr::n(), 
        wins = sum(result == "win")
      ) %>% 
      dplyr::mutate(
        pct = wins / games,
        color = map_chr(url, slowly(~mean_emoji_color(.x), rate_delay(1)))
        ) %>% 
      ggplot(aes(x = player, y = games, color = color, fill = color)) + 
      geom_bar(stat = "identity", alpha = .85) + 
      geom_text(aes(label = games), position = position_dodge(width = 0.9), vjust = -0.25) +
      labs(
        x = NULL,
        y = "Games Played",
        title = "Character Totals"
      ) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_minimal() +
      theme(
        axis.text.x = element_markdown()
        ) 
  })
})
