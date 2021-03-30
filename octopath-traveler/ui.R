material_page(
  title = "Octopath Traveler",
  nav_bar_color = "orange darken-4",
  tags$br(),
  
  # material_side_nav(
  #   fixed = FALSE,
  #   image_source = "octopath_traveler.jpg",
  #   background_color = NULL,
  #   div(
  #     p("Octopath Traveler was developed and published by Square Enix")
  #   )
  # ),
  
  material_tabs(
    tabs = c(
      "Introduction" = "zero_tab",
      "Character Explorer" = "first_tab",
      "Character Comparison Tool" = "second_tab"
      #"Nut Distributor (Coming soon)" = "third_tab"
    )
  ),
  
  material_tab_content(
    tab_id = "zero_tab",
    material_row(
      material_column(
        width = 12,
        material_card(
          depth = 4,
          div(
            h4("Overview"),
            p(
              tags$a("Octopath Traveler", href = "https://en.wikipedia.org/wiki/Octopath_Traveler"),
              " is a role-playing video game developed by Square Enix, originally released on July 13, 2018, and is currently available on PC and consoles."
            ),
            br(),
            h4("Character Explorer"),
            p("View character statistics from level 1-99"),
            br(),
            h4("Character Comparison Tool"),
            p("Compare stats for character pairs at a specific level."),
            br(),
            # h4("Nut Distributor (Coming Soon)"),
            # p("Allocate nuts and view the resulting impact on your characters."),
            # br(),
            h4("Acknowledgements"),
            p(
              "This app was motivated by ",
              tags$a("this reddit post", href = "https://www.reddit.com/r/octopathtraveler/comments/8wyzu0/octopath_base_level_stats_level_199_in_increments/"),
              " and the corresponding ",
              tags$a("data.", href = "https://docs.google.com/spreadsheets/d/1X-XO1bXJR90wMq3siW22ioERRKzgAUXob_yDpdpM25o/edit#gid=166759898")
            )
          )
        )
      )
      # material_column(
      #   width = 4,
      #   shiny::tags$img(src = "octopath_traveler.jpg", style = "width: 100%")
      # )
    )
  ),
  
  material_tab_content(
    tab_id = "first_tab",
    material_row(
      material_column(
        width = 2,
        material_card(
          title = "",
          depth = 4,
          material_radio_button(
            input_id = "stat",
            label = "Statistic",
            choices = unique(x$stat)
          ),
          material_slider(
            input_id = "level",
            label = "Level",
            min_value = 1,
            max_value = 99,
            initial_value = 1
          )
        )
      ),
      material_column(
        width = 10,
        material_card(
          depth = 4,
          plotlyOutput("plot_stat_vs_level")
        )
      ),
      material_column(
        width = 12,
        material_card(
          depth = 4,
          div(style = "font-size:75%",
            DT::dataTableOutput("table_stats")
          )
        )
      )
    )
  ),
  
  material_tab_content(
    tab_id = "second_tab",
    material_row(
      material_column(
        width = 4,
        material_card(
          depth = 4,
          material_slider(
            input_id = "level2",
            label = "Level",
            min_value = 1,
            max_value = 99,
            initial_value = 1
          )
        ),
        material_card(
          depth = 4,
          div(style = "font-size:75%",
            DT::dataTableOutput("table_similar")
          )
        )
      ),
      material_column(
        width = 8,
        material_card(
          title = "Character Similarity Matrix",
          depth = 4,
          plotlyOutput("plot_similar", height = 600)
        )
      )
    )
  )
  
)
