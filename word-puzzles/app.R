# GLOBAL --------------------------------------------

# load required packages
library(shiny)
library(worrrd)

# placeholder words
word_list <- c("insert", "your", "words", "here", "like", "this", "printing", "is", "cool", "too")
word_list <- paste(word_list, collapse = ",\n")
word_list


# UI ------------------------------------------------
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  titlePanel("Word Puzzle Generator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Setup"),
      radioButtons("game", label = "Choose a game:", choices = c("wordsearch", "crossword"), inline = T),
      column(width = 6, numericInput("rows", label = "Rows:", value = 10)),
      column(width = 6, numericInput("cols", label = "Columns:", value = 10)),
      textAreaInput("words", "Word List:", value = word_list, height = 300, resize = "both"),
      hr(),
      h3("Generate"),
      actionButton("generate", htmlOutput("words")),
      hr(),
      h3("Adjustments"),
      column(width = 6, checkboxInput("solution", "Show Solution")),
      column(width = 6, numericInput("size", label = "Letter Size", value = 8)),
      downloadButton("pdf", "Save (PDF)")
    ),
    
    mainPanel(
      uiOutput("plot_placeholder"),
      plotOutput("plot")
    )
  )
)


# SERVER ----------------------------------------
server <- function(input, output, session) {
  
  # parse input text
  words <- reactive({
    words <- stringr::str_split(toupper(input$words), ",")[[1]]
    stringr::str_replace_all(words, " |\n", "")
  })
  
  # generate puzzle (wordsearch/crossword)
  puzzle <- eventReactive(input$generate, {
    f <- eval(parse(text = input$game))
    f(words(), r = input$rows, c = input$cols)
  })
  
  puzzle_plot <- reactive({
    plot(puzzle(), solution = input$solution, letter_size = input$size)
  })
  
  # word stats
  output$words <- renderText({
    paste0("Create a ", input$game, " puzzle with ", length(words()), " words.")
  })
  
  # plot puzzle
  output$plot <- renderPlot(
    puzzle_plot()
  )
  
  output$plot_placeholder <- renderUI({
    #if (is.null(puzzle()))
    if (input$generate == 0) {
      div(
        h3("Patiently waiting for your puzzle..."),
        br(),
        tags$img(src = "https://media.giphy.com/media/1SvnHJFEuEH7hp81tF/giphy.gif", style = "width: 100%")
      )
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$pdf <- downloadHandler(
    filename = paste0(input$game, ".pdf"),
    content = function(file) {
      printable(puzzle_plot(), file)
    }
  )
  
  # can't download w/out a puzzle
  observe({
    shinyjs::disable("pdf")    # can't download on page-load
    shinyjs::disable("game")   # crossword not working yet!
    shinyjs::toggleState("pdf", !is.null(puzzle()) && length(puzzle()$words) > 0)
  })
}

shinyApp(ui, server)
