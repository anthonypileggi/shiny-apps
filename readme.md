# Shiny Apps

You may need to install and load some packages first:
```r
install.packages("shiny")
install.packages("shinydashboard")
install.packages("leaflet")
install.packages("plotly")
install.packages("magrittr")
install.packages("shinyjs")

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(ggplot2)
library(magrittr)
library(dplyr)
library(shinyjs)
```

You can run this app directly from github:
```r
# Run sampling-app
shiny::runGitHub(repo = "shiny-apps", username = "anthonypileggi", subdir = "sampling-app")

# Run quality-testing-app
shiny::runGitHub(repo = "shiny-apps", 
                 username = "anthonypileggi", 
                 subdir = "quality-testing-app")
```
