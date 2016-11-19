## todo -- display distribution when 
library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(shinyjs)

pvalueAddin <- function() {
  
  ui <- fluidPage(
            useShinyjs(),
            titlePanel("P-value Visualizer"),
            sidebarLayout(
              sidebarPanel(
                selectInput("distribution", label=h4("Distribution:"),
                  choices = c("Normal" = "normal", 
                    "Student's T" = "t", 
                    "Chi-Squared" = "x2", 
                    "F" = "f"),
                  selected = "normal"),
                div(id="t_params", h5("Parameters:"), numericInput("t_df", label="DF", value=100)),
                div(id="x2_params", h5("Parameters:"), numericInput("x2_df", label="DF", value=1)),
                div(id="f_params", h5("Parameters:"), numericInput("f_df1", label="Numerator DF", value=10),
                  numericInput("f_df2", label="Denominator DF", value=10)),
                br(),
                numericInput("test_statistic", label=h4("Test Statistic:"), value=2),
                selectInput("sides", label=h4("Alternative Hypothesis:"),
                  choices = c("Two-sided"="two",
                    "Less than (one-sided)" = "less",
                    "Greater than (one-sided)" = "greater"),
                  selected = "two_sided")
              ),
              mainPanel(
                plotOutput("density", height="200px"),
                verbatimTextOutput("code"),
                h4(textOutput("pvalue"), align="center"),
                actionButton("done", "Generate Code")
              )
            )
          )
          
  server <- function(input, output, session) {
    
    curDist <- reactive({
      input$distribution
    })
    
    densityRange <- reactive({
      qmin <- .001
      qmax <- 1-qmin
      switch(input$distribution,
        normal = qnorm(c(qmin,qmax)),
        t = qt(c(qmin,qmax),df=input$t_df),
        x2 = qchisq(c(0,qmax),df=input$x2_df),
        f = qf(c(0,qmax),df1=input$f_df1,df2=input$f_df2))
    })
    
    densityData <- reactive({
      xseq <- seq(densityRange()[1],densityRange()[2],by=.01)
      switch(input$distribution,
        normal = data.frame(x=xseq, y=dnorm(xseq)),
        t = data.frame(x=xseq, y=dt(xseq,df=input$t_df)),
        x2 = data.frame(x=xseq, y=dchisq(xseq,df=input$x2_df)),
        f = data.frame(x=xseq, y=df(xseq,df1=input$f_df1,df2=input$f_df2)))
    })
    
    areaData <- reactive({
      if (is.na(input$test_statistic)) {
        return(NULL)
      } else {
        switch(input$sides,
          greater = densityData() %>% dplyr::mutate(y = ifelse(x > input$test_statistic, y, 0)),
          less = densityData() %>% dplyr::mutate(y = ifelse(x < input$test_statistic, y, 0)),
          two = densityData() %>% dplyr::mutate(y = ifelse(x > abs(input$test_statistic)|x < -abs(input$test_statistic), y, 0)))
      }
    })
    
    pvalue <- reactive({
      switch(input$distribution,
        normal = switch(input$sides,
          greater = 1-pnorm(input$test_statistic),
          less = pnorm(input$test_statistic),
          two = 2*(1-pnorm(abs(input$test_statistic)))),
        t = switch(input$sides,
          greater = 1-pt(input$test_statistic, df=input$t_df),
          less = pt(input$test_statistic, df=input$t_df),
          two = 2*(1-pt(abs(input$test_statistic), df=input$t_df))),
        x2 = 1 - pchisq(input$test_statistic, df=input$x2_df),
        f = 1 - pf(input$test_statistic, df1=input$f_df1, df2=input$f_df2))
    })
    
    pvalueCode <- reactive({
      switch(input$distribution,
              normal = switch(input$sides,
                greater = paste0("1-pnorm(",input$test_statistic,")"),
                less = paste0("pnorm(",input$test_statistic,")"),
                two = paste0("2*(1-pnorm(abs(",input$test_statistic,")))")),
              t = switch(input$sides,
                greater = paste0("1-pt(",input$test_statistic,", df=",input$t_df,")"),
                less = paste0("pt(",input$test_statistic,", df=",input$t_df,")"),
                two = paste0("2*(1-pt(abs(",input$test_statistic,"), df=",input$t_df,"))")),
              x2 = paste0("1 - pchisq(",input$test_statistic,", df=",input$x2_df,")"),
              f = paste0("1 - pf(",input$test_statistic,", df1=",
                input$f_df1,",df2=",input$f_df2,")"))
    })
    
    output$code <- renderText({
      validate(
        need(pvalueCode(), 'Cannot provide a code snippet.')
      )
      pvalueCode()
    })
    
    output$pvalue <- renderText({
      validate(
        need(pvalue(), 'Not enough information to provide a p-value!')
      )
      paste0("P-value = ", round(pvalue(),3))
    })
    
    output$density <- renderPlot({
      req(densityData())
      g1 <- densityData() %>% ggplot(aes(x,y)) + 
                              geom_line() +
                              labs(x=NULL, y="Density") +
                              theme_bw()
      if (!is.null(areaData())) {
        g1 <- g1 + geom_area(aes(x=x, y=y), alpha=.5, fill='red', data=areaData())
      }
      g1
    })
    
    observe({
      toggle("t_params", condition=input$distribution=="t")
    })
    observe({
      toggle("x2_params", condition=input$distribution=="x2")
    })
    observe({
      toggle("f_params", condition=input$distribution=="f")
    })
    observe({
      toggleState("sides", condition=!(input$distribution=="f"|input$distribution=="x2"))
    })
    observe({
      toggleState("done", condition=!is.na(pvalue()))
    })
    observeEvent(input$distribution=="f" | input$distribution=="x2", {
      updateSelectInput(session, "sides", selected="greater")
    })
    
    observeEvent(input$done, {
      rstudioapi::insertText(pvalueCode())
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer("Pvalue Visualizer", width=1000))
}

