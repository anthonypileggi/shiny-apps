library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)


# number of mines
n <- 10


# Functions ======================

# draw game board
draw_board <- function(n, mines) {
  
  # randomly place mines (if placement not provided)
  if (is.numeric(mines)) {
    mines <- 
      tibble::tibble(
        id = sample(1:(n^2), size = mines, replace = F),
        mine = T
      )
  }
    
  expand.grid(i = 1:n, j = 1:n) %>% 
    dplyr::mutate(
      id = dplyr::row_number()
      ) %>%
    dplyr::left_join(mines, by = "id") %>%
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


# expand a click once
expand_click_once <- function(board, click) {
  
  if (click$count != 0)
    return(click)
  
  board %>%
    dplyr::filter(
      i >= click$i - 1,
      i <= click$i + 1,
      j >= click$j - 1,
      j <= click$j + 1
    )
}

# recursively expand a click
expand_click <- function(board, click) {
  ct <- 0
  new_click <- NULL
  while (!identical(click, new_click)) {
    ct <- ct + 1
    if (ct > 1)
      click <- new_click
    new_click <- 
      purrr::map_df(
        1:nrow(click), 
        function(i) expand_click_once(board, click[i, ])
      ) %>% 
      dplyr::distinct() %>%
      dplyr::arrange(id)
    print(nrow(new_click))
  }
  new_click
}

# test expansion
# board <- draw_board(10, 5) %>%
#   compute_counts()
# click <- head(dplyr::filter(board, !mine, count == 0), 1)
# new_click <- expand_click(board, click)


# BIGQUERY FUNCTIONS ======================================

# auth to bigquery
bq_auth <- function(token = Sys.getenv("GOOGLE_AUTH_FILE")) {
  if (token == "")
    stop("You must define GOOGLE_AUTH_FILE as location for JSON file.", call. = FALSE)
  if (!file.exists(token))
    stop(paste("No JSON file found at", token), call. = FALSE)
  bigrquery::bq_auth(path = token)
}

#' Append data to a new/existing BigQuery table
#' @param data data to append
#' @param dataset bigquery dataset being updated
#' @param table bigquery table being updated
#' @param project bigquery project
#' @export
bq_append <- function(data = NULL,
                      dataset = NULL,
                      table = NULL,
                      project = Sys.getenv("GOOGLE_PROJECT")) {
  
  # checks
  if (any(is.null(data), is.null(dataset), is.null(table)))
    stop("Must include non-missing values for fields {`data`, `dataset`, `table`}.")
  
  # auth
  bq_auth()
  
  # connect to dataset
  x <- bigrquery::bq_dataset(project, dataset)
  if (!bigrquery::bq_dataset_exists(x))
    bigrquery::bq_dataset_create(x)
  
  # connect to table
  xt <- bigrquery::bq_table(project, dataset, table)
  if (!bigrquery::bq_table_exists(xt))
    bigrquery::bq_table_create(xt, fields = data)
  
  # write to table
  bigrquery::bq_table_upload(xt, values = data, write_disposition = "WRITE_APPEND")
}
