library(shiny)
library(shinymaterial)
library(tidyverse)
library(DT)
library(plotly)

# load data
x <- read_csv("octopath-traveler-character-stats.csv", col_types = cols())


# compute character-similarity matrix -------
results <- 
  tibble::tibble(
    name1 = character(), 
    name2 = character(), 
    stat = character(), 
    diff = integer(), 
    r = double()
  )
for (stat in names(x)[-(1:2)]) {
  for (name1 in unique(x$name)) {
    x1 <- filter(x, name == name1)[[stat]]
    for (name2 in unique(x$name)) {
      x2 <- filter(x, name == name2)[[stat]]
      results <- dplyr::bind_rows(results,
        tibble::tibble(
          name1 = name1,
          name2 = name2,
          stat = stat,
          diff = sum(abs(x1 - x2)),
          r = cor(x1, x2)
        )
      )
    }
  }
}


# convert from wide-->long format
x <- tidyr::gather(x, stat, value, -name, -level)
