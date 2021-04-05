library(shiny)
library(shinyjs)
library(tidyverse)


# number of mines
n <- 10


# Functions ======================

# draw game board
draw_board <- function(n, mines) {
  expand.grid(i = 1:n, j = 1:n) %>% 
    mutate(id = row_number()) %>%
    left_join(mines, by = "id") %>%
    tidyr::replace_na(list(mine = F))
}

# compute N mines touching each box
compute_counts <- function(board) {
  x2 <- purrr::map_df(
    1:nrow(board),
    function(id) {
      tmp <- board[id, ]
      board %>%
        dplyr::filter(
          i >= tmp$i - 1,
          i <= tmp$i + 1,
          j >= tmp$j - 1,
          j <= tmp$j + 1
        ) %>%
        dplyr::summarize(
          count = sum(mine)
        ) %>%
        dplyr::mutate(
          id = id
        )
    }
  )
  
  dplyr::left_join(board, x2, by = "id")
}
