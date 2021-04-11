# A Mazing Game
# =================

# --- global ---

# install some packages from github
#remotes::install_github("ColinFay/nessy")
#remotes::install_github("Vessy/Rmaze")

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(Rmaze)
library(nessy)
library(magrittr)
library(tidygraph)
library(ggplot2)
library(cowplot)
library(magick)

# TODO:
# -- why does the direction-options fail later in the maze (does not match the plot!!!)
# ------ test: walk thru maze to see where/when this happens

# character for map (todo: customize this!)
logo_file <- "frog.png"


# --- ui ---
ui <- cartridge(
  title = "maze",
  subtitle = "a maze-ing game",
  tagList(
    shinyjs::useShinyjs(),
    uiOutput("menu"),
    uiOutput("game")
  ),
  footer = "Built by Anthony Pileggi"
)


# --- server ---
server <- function(input, output, session) {

  # reactives ---
  rv <- reactiveValues(loc = NULL)

  maze <- reactive({
    input$new_game
    rv$loc <- dplyr::tibble(i = 1, j = 1)
    m <- makeGraph(10, 10)
    makeMaze_dfs(m, inShiny = T)
  })

  maze_df <- reactive({
    req(maze())
    tidygraph::as_tbl_graph(maze())
  })

  edges <- reactive({
    req(maze_df())
    maze_df() %>%
      activate(edges) %>%
      as_tibble()
  })

  nodes <- reactive({
    req(maze_df())
    maze_df() %>%
      activate(nodes) %>%
      as_tibble() %>%
      dplyr::mutate(
        id = dplyr::row_number(),
        i = purrr::map_chr(name, ~stringr::str_split(.x, "_")[[1]][3]),
        j = purrr::map_chr(name, ~stringr::str_split(.x, "_")[[1]][2])
      )
  })

  # current location/node
  this_node <- reactive({
    req(nodes())
    this_node <- nodes() %>%
      dplyr::filter(
        i == rv$loc$i,
        j == rv$loc$j
      )
  })

  # possible moves (from current location)
  moves <- reactive({
    req(this_node())
    req(edges())
    print("getting moves...")
    edges() %>%
      dplyr::filter(
        (from == this_node()$id | to == this_node()$id),
        wall == "OFF"
      ) %>%
      dplyr::mutate(
        dir = purrr::map2_chr(
          from, to,
          function(id1, id2) {
            if (id2 == this_node()$id) {
              id2 <- id1
              id1 <- this_node()$id
            }
            start <- dplyr::filter(nodes(), id == id1)
            end <- dplyr::filter(nodes(), id == id2)
            dplyr::case_when(
              start$i == end$i & start$j < end$j ~ "up",
              start$i == end$i & start$j > end$j ~ "down",
              start$i < end$i & start$j == end$j ~ "right",
              start$i > end$i & start$j == end$j ~ "left"
            )
          }
        )
      )
  })

  base_plot <- reactive({
    req(maze())
    plotMaze(maze(), 10, 10, T)
  })

  this_plot <- reactive({
    req(base_plot())
    req(rv$loc)
    base_plot() +
     # theme_bw() +
      draw_image(logo_file, x = rv$loc$i, y = rv$loc$j, hjust = 1, vjust = 1, halign = .5, valign = .5)
  })

  # observers ----
  # -- movement --
  observeEvent(input$up, {
    rv$loc$j <- rv$loc$j + 1
  })
  observeEvent(input$down, {
    rv$loc$j <- rv$loc$j - 1
  })
  observeEvent(input$left, {
    rv$loc$i <- rv$loc$i - 1
  })
  observeEvent(input$right, {
    rv$loc$i <- rv$loc$i + 1
  })

  # only allow moves that do not cross walls
  observe({
    req(moves())
    print("MOVES")
    print(moves())
    purrr::map(
      c("up", "down", "left", "right"),
      function(d)
        shinyjs::toggleState(id = d, condition = d %in% moves()$dir)
    )

  })

  # is game over?
  observe({
    req(nodes())
    req(this_node())
    if (this_node()$id == max(nodes()$id)) {
      sendSweetAlert(
        session = session,
        title = "YOU WIN!",
        type = "success"
      )
    }
  })

  # outputs -----

  output$maze <- renderPlot({
    req(this_plot())
  })

  # Setup
  output$menu <- renderUI({
    container_with_title(
      title = "Setup",
      button_primary("new_game", "New Game"),
    )
  })

  # Maze Gameplay
  output$game <- renderUI({
    container_with_title(
      "Gameplay",
      container_with_title(
        "Controls",
        button("up", "Up"),
        button("down", "Down"),
        button("left", "Left"),
        button("right", "Right")
      ),
      container_with_title(
        "Maze",
        plotOutput("maze")
      )
    )
  })
}

shiny::shinyApp(ui, server)
