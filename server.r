library(DT)
library(shiny)
library(shinyjs)
library(shinyBS)
library(sodium)
library(stringi)
library(shinycssloaders)
library(flexdashboard)
library(shinydashboard)
library(shinyalert)

## import login page function
source("login_page.r")

##### Saved Credentials #####
# credentials = data.frame(
#     username_id = c("koushik@gmail.com", "sounak@gmail.com"),
#     passod   = c("koushik@gmail.com", "sounak@gmail.com"),
#     permission  = c("basic", "advanced"),
#     stringsAsFactors = F
# )



credentials = data.frame(
    username_id = c("koushik@gmail.com", "sounak@gmail.com"),
    passod = sapply(c("koushik@gmail.com", "sounak@gmail.com"), password_store),
    permission  = c("basic", "advanced"),
    stringsAsFactors = F
)

server <- function(input, output, session) {
    login = FALSE
    USER <- reactiveValues(login = login)
    
    ## Show the controls (action button for data) only when file is uploaded
    observeEvent(input$file,
                 shinyjs::show("data_b"))
    
    ## Show the controls (action button for plots) only when file is uploaded
    observeEvent(input$file,
                 shinyjs::show("plot_b"))
    
    output$plot1 <- renderPlot({
        # message(sprintf(
        #   "\nhttp://localhost:8000/iris?sp=%s, var=%s",
        #   input$sp,
        #   input$var
        # ))
        dtf <- jsonlite::fromJSON(sprintf("http://localhost:8000/iris?sp=%s", input$sp))
        ggplot2::ggplot(dtf, aes_string(input$var)) +
            geom_histogram(bins = 30) +
            theme_minimal()
    })
    
    
    observeEvent(input$randgen, {
        output$gauge = renderGauge({
            acc <- jsonlite::fromJSON(sprintf("http://localhost:8000/get_accuracy"))
            gauge(
                acc, 
                min = 0, 
                max = 1, 
                sectors = gaugeSectors(success = c(0.5, 1), 
                                       warning = c(0.3, 0.5),
                                       danger = c(0, 0.3)))
        })
    })
    
    
    
    observe({
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if (length(which(credentials$username_id == Username)) == 1) {
                        pasmatch  <-
                            credentials["passod"][which(credentials$username_id == Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if (pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(
                                id = "nomatch",
                                anim = TRUE,
                                time = 1,
                                animType = "fade"
                            )
                            shinyjs::delay(
                                3000,
                                shinyjs::toggle(
                                    id = "nomatch",
                                    anim = TRUE,
                                    time = 1,
                                    animType = "fade"
                                )
                            )
                        }
                    } else {
                        shinyjs::toggle(
                            id = "nomatch",
                            anim = TRUE,
                            time = 1,
                            animType = "fade"
                        )
                        shinyjs::delay(
                            3000,
                            shinyjs::toggle(
                                id = "nomatch",
                                anim = TRUE,
                                time = 1,
                                animType = "fade"
                            )
                        )
                    }
                }
            }
        }
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(
            a(icon("fa fa-sign-out"), "Logout",
              href = "javascript:window.location.reload(true)"),
            class = "dropdown",
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;"
        )
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE) {
            sidebarMenu(
                menuItem(
                    "Our Mission",
                    tabName = "mission",
                    icon = icon("dashboard")
                ),
                
                menuItem(
                    "Data Import",
                    tabName = "data_import",
                    icon = icon("dashboard")
                ),
                
                menuItem(
                    "Data Quality Check",
                    tabName = "data_check",
                    icon = icon("dashboard")
                ),
                
                menuItem(
                    "Modeling",
                    tabName = "modeling",
                    icon = icon("dashboard")
                ),
                
                menuItem(
                    "Reporting",
                    tabName = "reporting",
                    icon = icon("dashboard")
                )
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE) {
            tabItems(
                # content for "mission" tab
                tabItem(
                    tabName = "mission",
                    fluidRow(
                        box(
                            title = "Why waisDOM.AI?",
                            width = 6,
                            solidHeader = TRUE,
                            status = "primary",
                            # text placeholder
                            stri_rand_lipsum(1)
                        ),
                        
                        box(
                            title = "Analytics people love to use",
                            width = 6,
                            solidHeader = TRUE,
                            status = "primary",
                            # text placeholder
                            stri_rand_lipsum(1)
                        )
                    )
                ),
                
                # content for "data_import" tab
                tabItem(
                    tabName = "data_import",
                    
                    # box for file upload 
                    box(
                        width = 5,
                        title = "Upload local file",
                        status = "primary",
                        solidHeader = TRUE,
                        fileInput("file", "Browse for data files"),
                        
                        ## View data action button hidden initially until the dataset is loaded
                        shinyjs::hidden(
                            div(
                                id = "view_data_btn",
                                style = "display:inline-block",
                                actionButton("data",  "View Data", icon = icon('table'))
                            )
                        ),
                        
                        ## View plot action button hidden initially until the dataset is loaded
                        shinyjs::hidden(
                            div(
                                id = "plot_data_btn",
                                style = "display:inline-block",
                                actionButton("plot",  "View Plot", icon = icon("bar-chart"))
                            )
                        ),
                        
                        ## Shiny BS Modal to display the dataset inside a modal
                        ## A spinner is also added
                        bsModal(
                            id = "dataset",
                            title = "This is you have just uploaded",
                            trigger = "data",
                            size = "large",
                            withSpinner(dataTableOutput("data_set"))
                        ),
                        
                        ## Shiny BS Modal to display the plot inside a modal
                        ## A spinner is also added
                        bsModal(
                            id = "Plot",
                            title = "Plot",
                            trigger = "plot",
                            size = "large",
                            sliderInput(
                                inputId = "b",
                                label = "Select the bin width value" ,
                                min = 50  ,
                                max = 500,
                                value = 100
                            )
                            # varSelectInput("variable", "Variable:", mtcars)
                        )
                    ),
                    
                    # box for database import
                    box(
                        width = 5,
                        title = "Connect to database",
                        status = "primary",
                        solidHeader = TRUE,
                        textInput("db_user_name", "Database User Name"),
                        textInput("db_passwd", "Password"),
                        textInput("db_name", "Database Name"),
                        column(
                            width = 12,
                            splitLayout(
                                cellWidths = c("50%", "50%"),
                                actionButton(
                                    inputId = "db_connect_btn", 
                                    label = "Connect", 
                                    icon = icon("refresh"),
                                    style = "width:100px"
                                ),
                                
                                actionButton(
                                    inputId = "ping_test_btn", 
                                    label = "Ping Test",
                                    style = "width:100px"
                                )
                            )
                        )
                    )
                )
            )
        } else {
            loginpage
        }
    })
    
    ## Reactive function to read data from uploaded CSV file
    ## Below code can be customized based on the app / user needs and layout/type of the file
    data_uploaded <- reactive({
        file1 <- input$file
        if (is.null(file1)) {
            return()
        }
        read.table(
            file = file1$datapath,
            sep = ",",
            header = T,
            stringsAsFactors = T
        )
        
    })
    
    ## Rendering uploaded data to a datatable
    output$data_set <-
        renderDataTable(data_uploaded(), options = list(scrollX = TRUE))
    
    ## Create and render ggplot
    ## Below code can be customized based on the app / user needs for plot
    output$plot_gg <- renderPlot(
        ## plotting histogram of diamonds price
        ggplot(data = data_uploaded()) +
            geom_histogram(binwidth = input$b, aes(x = price)) +
            ggtitle("Diamond Price Distribution") +
            xlab(paste("Diamond Price & binwidth as ", input$b)) +
            ylab("Frequency") +
            theme_minimal() + xlim(0, 2500)
    )
    
    # output$results <-  DT::renderDataTable({
    #   datatable(
    #       iris,
    #       options = list(
    #           autoWidth = TRUE,
    #           searching = FALSE
    #           )
    #   )
    # })
}

# runApp(list(ui = ui, server = server), launch.browser = TRUE)