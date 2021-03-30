
function(input, output, session) {
  
  # Reactives ---------------------------
  
  data <- reactive({
    x %>%
      dplyr::filter(
        stat %in% input$stat
      )
  })
  
  # respond to plotly click
  selected_level <- reactive({
    event_data("plotly_click")$x
  })
  observeEvent(selected_level(), {
    update_material_slider(session, "level", value = selected_level())
  })
  
  
  # Outputs --------------------------

  output$plot_stat_vs_level <- renderPlotly({
    g <- data() %>%
      mutate(
        text = stringr::str_c(
          name,
          "<br>Level ",
          level,
          "<br>",
          input$stat,
          " = ",
          scales::comma(value)
        )
      ) %>%
      ggplot(aes(x = level, y = value, color = name, group = name)) + 
      geom_line(aes(text = text)) + 
      geom_vline(xintercept = as.numeric(input$level), linetype = "dashed") +
      labs(
        x = "Level", y = NULL, color = "Character",
        title = input$stat
        ) +
      scale_y_continuous(labels = scales::comma) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = .5)
      )

    ggplotly(g, tooltip = "text")
  })
  
  # this is not used right now...
  output$plot_stats_this_level <- renderPlot({
    x %>% 
      dplyr::filter(level == input$level) %>% 
      ggplot(aes(x = name, y = value, fill = name)) + 
      geom_bar(stat = "identity") + 
      labs(x = NULL, y = NULL, fill = NULL) +
      facet_wrap(~stat, nrow = 10, scales = "free_x") + 
      coord_flip() + 
      theme_void() + 
      theme(legend.position = "bottom")
  })
  
  output$table_stats <- DT::renderDataTable({
    x %>% 
      dplyr::filter(level == input$level) %>% 
      dplyr::select(-level) %>% 
      tidyr::spread(stat, value) %>%
      DT::datatable(
        rownames = FALSE,  
        options = list(dom = "t", pageLength = 8)
        ) %>%
      DT::formatStyle(
        input$stat,
        backgroundColor = "orange"
      ) %>%
      DT::formatRound(unique(x$stat), digits = 0)
  })

  
  # Character Comparison Tool -------------
  
  output$table_similar <- DT::renderDataTable({
    req(selected_chars())
    c1 <- selected_chars()[1]
    c2 <- selected_chars()[2]
    xt <- x %>% 
      filter(
        level == input$level2, 
        name %in% selected_chars()
        ) %>% 
      select(-level) %>% 
      spread(name, value)
    xt$Diff <- xt[[c1]] - xt[[c2]]
    
    DT::datatable(
      xt,
      rownames = FALSE,  
      options = list(dom = "t", pageLength = 10)
    ) %>%
      DT::formatRound(selected_chars(), digits = 0)
  })
  
  output$plot_similar <- renderPlotly({
    g <- results %>% 
      group_by(name1, name2) %>% 
      summarize(same = sum(diff == 0)) %>% 
      arrange(same) %>% 
      ggplot(aes(x = name1, y = name2, fill = same)) + 
      geom_tile() + 
      geom_text(aes(label = same)) + 
      scale_fill_gradient(low = "white", high = "green", guide = FALSE) + 
      labs(x = NULL, y = NULL) + 
      theme_bw()
    ggplotly(g, tooltip = "text", source = "similar")
  })
  
  # respond to plotly click
  selected_chars <- reactive({
    s <- event_data("plotly_click", source = "similar")
    if (!is.null(s)) {
      unique(x$name)[c(s$x, s$y)]
    } else {
      c("Alfyn", "Cyrus")
    }
  })
}
