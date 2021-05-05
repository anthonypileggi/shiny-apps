library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinytoastr)
library(shinycssloaders)
library(shinymanager)

source("sputnik_funs.R")


# Keywords
keywords <- c(
  "Turkey", "Turkiye", "USA", "ABD", "Russia", "Rusya", "covid19",
  "vaccine", "aşı", "J&J", "Johnson and Johnson", "kan pıhtıları",
  "clots", "Moderna", "Pfizer", "AstraZeneca", "China", "Çin"
)

# Credentials
credentials <- data.frame(
  user = c("sam", "anthony", "scmcclintock"),
  password = scrypt::hashPassword("12345"),
  is_hashed_password = TRUE,
  comment = "user",
  stringsAsFactors = FALSE
)

# UI ------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$img(
        src = 'logo_light.png',
        title = "Midsteam Technologies", 
        height = "40px",
        style = "float: left; padding-top:10px;"
      ),
      "Aletheia"
    ),
    tags$li(
      tags$a(
        href = "http://midstream.us/",
        tags$img(
          src = 'logo_light.png',
          title = "Midsteam Technologies", 
          height = "30px"
        ),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome", tabName = "welcome", icon = icon("bullhorn")),
      menuItem("Reports", tabName = "reports", icon = icon("flag")),
      menuItem("Keywords", tabName = "keywords", icon = icon("search")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs")),
      menuItem("Log Out", tabName = "logout", icon = icon("sign-out"))
    )
  ),
  dashboardBody(
    
    auth_ui(
      id = "auth",
      # add image on top ?
      tags_top = 
        tags$div(
          tags$h4("Midstream Technology: Aletheia Access", style = "align:center"),
          tags$img(
            src = "logo.png", width = 100
          )
        ),
      # add information on bottom ?
      tags_bottom = tags$div(
        tags$p("Two-Factor Authentication is Enabled"),
        tags$p(
          "For any question, please  contact the ",
          tags$a(
            href = "mailto:scmcclintock@gmail.com?Subject=Disinformation-App",
            target="_top", "admin"
          ),
          " or call ",
          tags$a(
            href = "tel:757-705-9200",
            target="_top", "(757) 705-9200"
          ),
        )
      )
    ),
    
    tabItems(
      
      tabItem(
        tabName = "welcome",
        h2("Welcome to Aletheia"),
        tags$hr(),
        tags$p(tags$strong("Analysts: "), "please select from the menu at left"),
        tags$br(),
        tags$br(),
        tags$p(tags$strong("Last Report Run: "), "02May2021"),
        tags$p(tags$strong("Last Setting Change: "), "     28Apr2021"),
        tags$p(tags$strong("Latest Build: "), "16Apr2021, Vers. 8.1"),
        tags$br(),
        tags$p(tags$strong("Alerts as of 05May2021: "), "  None")
      ),
      
      tabItem(
        tabName = "reports",
        box(
          width = 4,
          title = "Choose Report",
          selectizeInput("report", label = "", choices = format(as.Date(c("2021-04-24", "2021-04-30", "2021-05-04")), by = "day")),
          downloadButton("download", "Download (ZIP)")
        ),
        tabBox(
          id = "results_tabs",
          width = 8,
          tabPanel(
            "Data",
            DT::dataTableOutput("data")
            ),
          tabPanel(
            "Filtered", 
            DT::dataTableOutput("filtered")
            ),
          tabPanel(
            "Network 1", 
            tags$img(src = "sputnik_network.png", width = "100%")
            ),
          tabPanel(
            "Network 2", 
            tags$img(src = "network.png", width = "100%")
          ),
          tabPanel(
            "Virality", 
            tags$img(src = "virality.png", width = "100%")
            ),
          tabPanel("Alerts", "None")
        )
      ),
      
      tabItem(
        tabName = "keywords",
        box(
          width = 4,
          title = "Edit Keywords/Phrases",
          tags$span("(must have edit privileges)"),
          textInput("keywords", label = ""),
          actionButton("add", "Add"),
          actionButton("remove", "Remove")
        ),
        box(
          width = 8,
          title = "Current Keywords and Phrases to Target",
          column(width = 6, purrr::map(keywords[1:9], tags$p)),
          column(width = 6, purrr::map(keywords[10:18], tags$p))
        )
        # box(
        #   width = 4,
        #   title = "Search Keywords",
        #   textInput("keywords", label = "Keywords", placeholder = "covid, usa, biden, ..."),
        #   uiOutput("keywords")
        # ),
        # box(
        #   width = 8,
        #   title = uiOutput("search_title"),
        #   DT::dataTableOutput("search")
        # )
      ),
      
      tabItem(
        tabName = "settings",
        h2("Settings")
      )
    )
  )
)

server <- function(input, output) {
  
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )
  
  rv <- reactiveValues(
    #data = readr::read_csv("data.csv"), 
    data = readr::read_csv("sputnikStoryTweets.csv"),
    filtered = readxl::read_xlsx("koronavirus_filtered_w_tweet_urls.xlsx"), 
    keywords = NULL
    )
  
  # observeEvent(input$keywords, {
  #   rv$keywords <- stringr::str_trim(stringr::str_split(input$keywords, ",")[[1]])
  #   rv$keywords <- rv$keywords[nchar(rv$keywords) > 0]
  #   rv$filtered <- filter_sputnik_articles(rv$data, rv$keywords)
  # })
  
  # output$search <- DT::renderDataTable({
  #   req(rv$filtered)
  #   rv$filtered %>%
  #     dplyr::arrange(desc(count)) %>%
  #     dplyr::transmute(
  #       Page = page,
  #       Title = purrr::map2_chr(title, url, ~as.character(tags$a(.x, href = .y))),
  #       Date = date,
  #       Terms = found,
  #       Hits = count
  #     ) %>%
  #   DT::datatable(
  #     rownames = FALSE,
  #     escape = FALSE,
  #     selection = "multiple",
  #     filter = "none",
  #     options = list(
  #       dom = "lftp",
  #       scrollX = TRUE,
  #       pageLength = 10
  #     )
  #   )
  # })
  
  output$keywords <- renderUI({
    list(
      tags$strong(paste0("Searching for ", length(rv$keywords), " keywords:")),
      purrr::map(rv$keywords, tags$p)
    )
  })
  
  output$search_title <- renderUI({
    paste0(
      "Results - Found ",
      ifelse(is.null(rv$filtered), 0, nrow(rv$filtered)),
      " / ",
      nrow(rv$data),
      " links"
    )
  })
  
  output$data <- DT::renderDataTable({
    req(rv$data)
    rv$data %>%
      DT::datatable(
        rownames = FALSE,
        escape = FALSE,
        selection = "multiple",
        filter = "none",
        options = list(
          dom = "lftp",
          scrollX = TRUE,
          pageLength = 10
        )
      )
  })
  
  output$filtered <- DT::renderDataTable({
    req(rv$filtered)
    rv$filtered %>%
      DT::datatable(
        rownames = FALSE,
        escape = FALSE,
        selection = "multiple",
        filter = "none",
        options = list(
          dom = "lftp",
          scrollX = TRUE,
          pageLength = 10
        )
      )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("aletheia-report-", input$report, ".zip")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in c(1,2,3,4,5)) {
        path <- paste0("sample_", i, ".csv")
        fs <- c(fs, path)
        write(i*2, path)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$tabs, {
    if (input$tabs == "logout")
      stopApp()
  })
}


shinyApp(ui, server)
