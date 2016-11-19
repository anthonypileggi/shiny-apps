library(shiny)

shinyServer(function(input, output) {
   
  # Draw from distribution
  draws <- reactive({
    estimateStoppingCost(items=input$items, 
                          n=input$n, 
                          x=input$x, 
                          fail_cost=input$fail_cost, 
                          labor_cost=20/input$labor_speed, 
                          B=100000)
  })
  
  # Estimate the number of tests until we can stop
  stopTime <- reactive({
    estimateStoppingTime(items=input$items, 
                          n=input$n, 
                          x=input$x, 
                          fail_cost=input$fail_cost, 
                          labor_cost=20/input$labor_speed)
  })
  
  # Plot `cost of stopping` distribution 
  output$ggPlot <- renderPlotly({
    g1 <- data.frame(xd=draws()) %>% ggplot(aes(x=xd)) + geom_density()
    tmp <- ggplot_build(g1)$data[[1]]

    g1 <- tmp %>% 
        ggplot() + 
          geom_area(data=subset(tmp,x<0), aes(x,density),fill="darkgreen",alpha=.5) +
          geom_area(data=subset(tmp,x>0), aes(x,density),fill="darkred",alpha=.5) +
          geom_vline(xintercept=0,linetype="dashed",color='red') +
          scale_x_continuous(labels=scales::dollar) +
          scale_y_continuous(labels=scales::percent) +
          labs(title="Cost of Stopping Tests Early", x="Stopping Cost", y=NULL) +
          theme_bw()
    ggplotly(g1, tooltip="x")
                      
  })
  
  # Displays
  output$textStopTime <- 
    renderText({ifelse(stopTime()$stop_time==input$items, 
                       paste0("You'll need to test the entire batch."),
                       ifelse(stopTime()$stop_time<=1,
                              paste0("You can stop testing now!"),
                              paste0("We can safely stop testing if the next ", round(stopTime()$stop_time), " parts don't fail.")))})
  
  output$textStopProb <- 
    renderText({ifelse(stopTime()$stop_time<=1,
                       "",
                       paste0("At an estimated ", round(100*stopTime()$current_phat,3), 
                              "% failure rate, likelihood of that many tests without a failure: ", 
                              round(100*stopTime()$stop_prob), "%"))})
  
  output$textMean <- renderText({ paste0("Most Likely Scenario: ", 
                                        ifelse(mean(draws())>0,"Lose","Save"), " $",
                                        format(round(abs(mean(draws()))),big.mark = ","))})
  output$textMax <- renderText({ paste0("Worst-case Scenario: ", 
                                        ifelse(max(draws())>0,"Lose","Save"), " $",
                                      format(round(abs(max(draws()))),big.mark = ","))})
  output$textMin <- renderText({paste0("Best-case Scenario: ", 
                                        ifelse(min(draws())>0,"Lose","Save"), " $",
                                      format(round(abs(min(draws()))),big.mark = ","))})
  
})
