library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinyBS)
library(sodium)
library(shinycssloaders)
library(flexdashboard)

# Login screen
loginpage <-
  div(
    id = "loginpage",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 2px;",
    wellPanel(
      tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:400;"),
      div(
        style = "display: inline-block;",
        img(
          src = "https://i.postimg.cc/9Q0t5Gxd/waisdom.png",
          height = 300,
          width = 460
        )
      ),
      textInput(
        "userName",
        placeholder = "Write Your Email",
        label = tagList(icon("user"), "Email")
      ),
      passwordInput(
        "passwd",
        placeholder = "Password",
        label = tagList(icon("unlock-alt"), "Password")
      ),
      br(),
      div(
        style = "text-align: center;",
        actionButton(
          "login",
          "SIGN IN",
          style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"
        ),
        shinyjs::hidden(div(
          id = "nomatch",
          tags$p(
            "Oops! Incorrect username or password!",
            style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
            class = "text-center"
          )
        )),
        br(),
        br(),
        p("Not having account?"),
        a("Please Sign Up Here", href = "https://www.google.com/")
        # tags$code("Username: koushik@gmail.com  Password: koushik@gmail.com"),
        # br(),
        # tags$code("Username: sounak@gmail.com  Password: sounak@gmail.comypass1")
      )
    )
  )


credentials = data.frame(
  username_id = c("koushik@gmail.com", "sounak@gmail.com"),
  passod = sapply(c("koushik@gmail.com", "sounak@gmail.com"), password_store),
  permission  = c("basic", "advanced"),
  stringsAsFactors = F
)

header <- dashboardHeader(title = "WaisDOM", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
# ,menuItem("Sign Up Page", tabName = "signUp", icon = icon("dashboard"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
# tabItem(tabName = "signUp",
#         h2("Can input missing Data")
# )
ui <- dashboardPage(header, sidebar, body, skin = "green")



##### Server Code #####
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
          "Main Page",
          tabName = "dashboard",
          icon = icon("dashboard")
        ),
        menuItem(
          "Import Data",
          tabName = "importData",
          icon = icon("th")
        ),
        menuItem(
          "Data Exploration",
          tabName = "dataExploration",
          icon = icon("dashboard"),
          menuSubItem(
            "Raw Data Presentation",
            tabName = "rawData",
            icon = icon("list-alt")
          ),
          menuSubItem(
            "Data Entry",
            tabName = "dataEntry",
            icon = icon("cog")
          ),
          menuSubItem(
            "Updated Data Presentation",
            tabName = "updatedData",
            icon = icon("refresh")
          )
        ),
        menuItem(
          "Data Visualization - 1",
          tabName = "dataViz_1",
          icon = icon("dashboard")
        ),
        menuItem(
          "Data Visualization - 2",
          tabName = "dataViz_2",
          icon = icon("dashboard")
        )
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      tabItems(
        tabItem(
          tabName = "dashboard",
          h3("Welcome To waisDOM.AI"),
          br(),
          br(),
          fluidRow(
            box(
              title = "Why waisDOM.AI?",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              "As the market-leading choice for modern business intelligence,
              the waisDOM platform is known for taking any kind of data from
              almost any system, and turning it into actionable insights with
              speed and ease. It's as simple as dragging and dropping.
              And on our mission to help people see and understand data,
              we go beyond our technology to ensure customer success by
              helping people build a data culture."
            ),
            
            box(
              title = "Analytics people love to use",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              "As the market-leading choice for modern business intelligence,
              the waisDOM platform is known for taking any kind of data from
              almost any system, and turning it into actionable insights with
              speed and ease. It's as simple as dragging and dropping.
              And on our mission to help people see and understand data,
              we go beyond our technology to ensure customer success by
              helping people build a data culture."
            ),
          ),
          br(),
          br(),
          fluidRow(column(
            12,
            align = "center",
            br(),
            box(
              title = "Analytics people love to use",
              width = 6,
              solidHeader = TRUE,
              status = "warning",
              "With built-in visual best practices, waisDOM enables limitless
              visual data exploration without interrupting the flow of analysis."
            ),
            
            div(
              style = "display: inline-block;",
              img(
                src = "https://cdns.tblsft.com/sites/default/files/pages/platform-3-800x5002x.jpg",
                height = 300,
                width = 500
              )
            ),
          )),
          br(),
          br(),
          
          fluidRow(
            column(4,
                   wellPanel(
                     h3("Flexibility and choice"),
                     p(
                       "With waisDOM, you can leverage your existing technology
                       investments and rest assured we'll grow with you as the
                       data landscape evolves. We offer the most options to
                       deploy analytics and connect to all of your data,
                       no matter where it resides."
                     )
                   )),
            column(4, wellPanel(
              h3("Governance and security"),
              p(
                "waisDOM empowers you with the enterprise-grade security and
                governance models to keep data in the right hands at all times,
                even when scaling analytics across your organization.
                Plus, administration is simple and powerful,
                helping IT focus on what matters most."
              )
            )),
            column(4, wellPanel(
              h3("Integration and extensibility"),
              p(
                "With our breadth and depth of capabilities, coupled with
                our vast partner network, Tableau serves global enterprises
                across the full cycle of self-service analytics-from prep to
                analysis to sharing, with governance and data management every
                step of the way."
              )
            ))
          ),
          
          fluidRow(div(
            style = "display: inline-block;",
            img(
              src = "https://images.pexels.com/photos/373543/pexels-photo-373543.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940",
              height = 360,
              width = "100%"
            )
          ), ),
        ),
        
        tabItem(
          tabName = "importData",
          
          h2("Import Your Data"),
          
          br(),
          
          ## Box
          box(
            width = 5,
            title = "Input Data",
            status = "primary",
            solidHeader = TRUE,
            fileInput("file", "Upload Data"),
            
            ## View data action button hidden initially until the dataset is loaded
            shinyjs::hidden(
              div(
                id = "data_b",
                style = "display:inline-block",
                actionButton("data",  "View Data", icon = icon('table'))
              )
            ),
            
            ## View plot action button hidden initially until the dataset is loaded
            shinyjs::hidden(
              div(
                id = "plot_b",
                style = "display:inline-block",
                actionButton("plot",  "View Plot", icon = icon("bar-chart"))
              )
            )
          ),
          
          ## Shiny BS Modal to display the dataset inside a modal
          ## A spinner is also added
          bsModal(
            id = "dataset",
            title = "Diamonds Dataset",
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
            ),
            br(),
            withSpinner(plotOutput("plot_gg"))
          ),
        ),
        
        tabItem(
          tabName = "rawData",
          h2("Original Input Data Representation"),
          br(),
          p("Explore the data"),
          br(),
          p("P/D Algo & Forecasting page")
        ),
        
        tabItem(
          tabName = "dataEntry",
          h2("Can input missing Data"),
          br(),
          p("Update the input data"),
        ),
        
        tabItem(
          tabName = "updatedData",
          h2("Updated Data Representation"),
          br(),
          p("Explore the data"),
        ),
        
        tabItem(
          tabName = "dataViz_1",
          h2("Data Visualization"),
          br(),
          p("Get insight of your data through visualization"),
          fluidRow(
            title = "Test API",
            column(
              3,
              selectInput("sp", "Species", choices = unique(iris$Species)),
              selectInput("var", "Variable", choices = names(iris)[-5])
            ),
            column(9,
                   plotOutput("plot1"))
          )
        ),
        
        tabItem(
          tabName = "dataViz_2",
          h2("Data Visualization 2"),
          br(),
          p("Get insight of your data through visualization"),
          
          fluidRow(
            actionButton("randgen", "Get Model Accuracy"),
            gaugeOutput("gauge")
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

runApp(list(ui = ui, server = server), launch.browser = TRUE)