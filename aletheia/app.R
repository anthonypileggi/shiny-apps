library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinytoastr)
library(shinycssloaders)
library(shinymanager)

source("sputnik_funs.R")

# Credentials
credentials <- data.frame(
  user = c("sam", "anthony"),
  password = c(scrypt::hashPassword("12345"), scrypt::hashPassword("12345")),
  is_hashed_password = TRUE,
  comment = c("admin", "user"),
  stringsAsFactors = FALSE
)

# UI ------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Aletheia",
    tags$li(
      tags$a(
        href = "http://midstream.us/",
        tags$img(
          src = 'logo.png',
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
      menuItem("Welcome", tabName = "welcome", icon = icon("bullhorn")),
      menuItem("Reports", tabName = "reports", icon = icon("flag")),
      menuItem("Keywords", tabName = "keywords", icon = icon("search")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    
    auth_ui(
      id = "auth",
      # add image on top ?
      tags_top = 
        tags$div(
          tags$h4("Midstream Technology & WMLA", style = "align:center"),
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
        tags$p(tags$strong("Analysts: "), "Please select either Reports or Settings from the menu at left."),
        tags$p(tags$strong("Last Report Run: "), "02May2021"),
        tags$p(tags$strong("Last Setting Change: "), "     28Apr2021"),
        tags$p(tags$strong("Latest Build: "), "16Apr2021, Vers. 8.1"),
        tags$p(),
        tags$p(tags$strong("Alerts as of 05May2021: "), "  None")
      ),
      
      tabItem(
        tabName = "reports",
        box(
          width = 4,
          title = "Choose Report",
          selectizeInput("report", label = "", choices = c(format(seq(Sys.Date() - 7, Sys.Date(), by = "day"))))
        ),
        tabBox(
          id = "results_tabs",
          width = 8,
          tabPanel("Data", "All Articles w/ links"),
          tabPanel("Filtered", "Subset of data"),
          tabPanel(
            "Network", 
            tags$img(src = "network.png", width = "100%")
            ),
          tabPanel("Virality", "Exposure Weighting and Propagation Rates")
        )
      ),
      
      tabItem(
        tabName = "keywords",
        box(
          width = 4,
          title = "Search Keywords",
          textInput("keywords", label = "Keywords", placeholder = "covid, usa, biden, ..."),
          uiOutput("keywords")
        ),
        box(
          width = 8,
          title = uiOutput("search_title"),
          DT::dataTableOutput("search")
        )
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
  
  rv <- reactiveValues(data = readr::read_csv("data.csv"), filtered = NULL, keywords = NULL)
  
  observeEvent(input$keywords, {
    rv$keywords <- stringr::str_trim(stringr::str_split(input$keywords, ",")[[1]])
    rv$keywords <- rv$keywords[nchar(rv$keywords) > 0]
    rv$filtered <- filter_sputnik_articles(rv$data, rv$keywords)
  })
  
  output$search <- DT::renderDataTable({
    req(rv$filtered)
    rv$filtered %>%
      dplyr::arrange(desc(count)) %>%
      dplyr::transmute(
        Page = page,
        Title = purrr::map2_chr(title, url, ~as.character(tags$a(.x, href = .y))),
        Date = date,
        Terms = found,
        Hits = count
      ) %>%
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
}


shinyApp(ui, server)
