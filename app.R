## import `setup_packages.r`
source("setup_packages.r")

## app.R ##
library(DT)
library(here)
library(shiny)
library(shinyjs)
library(shinyBS)
library(sodium)
library(jsonlite)
library(shinycssloaders)
library(flexdashboard)
library(shinydashboard)
library(shinyalert)
library(rAmCharts)



# source "run-api.r" to activate backend apis
# system("R CMD BATCH run_api.r")

# backend api health check
tryCatch (
    {
        ping_reply <- jsonlite::fromJSON(sprintf("http://localhost:8000/ping"))
        
        print("API is live!")
        
        # get dashboard root directory
        root <- here::here()
        
        # run dashboard
        runApp(root)
    },
    
    error = function(msg) {
        url <- "http://localhost:8000/ping"
        message(paste("URL does not seem to exist:", url))
        return (NA)
    }
)
