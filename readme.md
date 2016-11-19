# Shiny Apps

You may need to install some packages first:
```r
install.packages(shiny)
install.packages(shinydashboard)
install.packages(leaflet)
install.packages(plotly)
```

You can run this app directly from github:
```r
# Run sampling-app
shiny::runGitHub(repo = "shiny-apps", username = "anthonypileggi", subdir = "sampling-app")

# Run quality-testing-app
shiny::runGitHub(repo = "quality-testing-app", 
                 username = "anthonypileggi", 
                 subdir = "sampling-app")
```
