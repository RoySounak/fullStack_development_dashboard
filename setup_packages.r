# Setup Environment

pkg_list <- c(
    "DT",
    "shiny",
    "shinyjs",
    "shinyBS",
    "shinydashboard",
    "flexdashboard",
    "sodium",
    "shinycssloaders",
    "shinyalert",
    "ggplot2",
    "dplyr",
    "here",
    "rAmCharts",
    "rAmCharts4"
)

setup_packages <- function(package_list = pkg_list) {
    for (each in package_list) {
        if (!each %in% installed.packages()) {
            install.packages(
                each, 
                dependencies = T, 
                repos = "http://cran.us.r-project.org")
        } else {
            print(paste0(each, " found in your system"))
        }
    }
}

# call setup_packages()
setup_packages()