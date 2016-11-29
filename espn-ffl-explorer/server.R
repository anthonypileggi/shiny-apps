library(shiny)


shinyServer(function(input, output) {
   
  output$leagueInfo <- renderText({
      paste0("Currently in Week ",req(values$current_week))
    })
  
  ## scrape league data from espn
  values <- reactiveValues()
  observeEvent(input$fetch, {
    cat("Scraping from ESPN\n")
    values$x <- getLeagueData(leagueID=input$id)
    values$current_week <- getCurrentWeek(values$x)
    values$standings <- makeLeagueStandings(values$x)
    values$power_rankings <- makePowerRankings(values$x)
  })

  ## League Standings
  output$standings <- DT::renderDataTable(req(values$standings), 
                                          server = FALSE,
                                          options = list(dom='t',
                                                         pageLength = dim(values$standings)[1],
                                                         searching=FALSE))
  
  ## Power Rankings
  output$power_rankings <- DT::renderDataTable(req(values$power_rankings), 
                                               server = FALSE,
                                               options = list(dom='t',
                                                              pageLength = dim(values$power_rankings)[1],
                                                              searching=FALSE))
  
  ## Scoring distribution by team
  output$plotScores <- renderPlotly({
    
    g1 <- req(values$x$results) %>% dplyr::filter(week < values$current_week) %>%
                               ggplot(aes(team,score)) + 
                                geom_boxplot(fill='dodgerblue') +
                                geom_hline(yintercept=median(x$results$score[x$results$score!=0]), 
                                            linetype='dashed', color='red', alpha=.5) +
                                labs(x=NULL, y="Weekly Scores", 
                                     title=paste0("Through ", values$current_week," Weeks")) +
                                scale_y_continuous(labels=scales::comma) +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(g1)
    
  })
  
  ## Matchup Visualization
  output$plotMatchups <- renderPlotly({
    
    g1 <- req(values$x$matchups) %>% 
                dplyr::filter(week < values$current_week) %>%
                ggplot(aes(team1,team2)) + 
                  geom_tile(aes(fill=score2-score1), alpha=.5) +
                  geom_text(aes(label=round(score2-score1))) +
                  scale_fill_gradient2() +
                  labs(x="Home", y="Away", fill="Scores\n(Away-Home)") + 
                  theme_bw() +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1))
                        
    ggplotly(g1)
    
  })
})
