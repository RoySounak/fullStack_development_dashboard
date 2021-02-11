# Login screen
source("login_page.r")

# credentials = data.frame(
#     username_id = c("koushik@gmail.com", "sounak@gmail.com"),
#     passod = sapply(c("koushik@gmail.com", "sounak@gmail.com"), password_store),
#     permission  = c("basic", "advanced"),
#     stringsAsFactors = F
# )

header <- dashboardHeader(title = "WaisDOM", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
# ,menuItem("Sign Up Page", tabName = "signUp", icon = icon("dashboard"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
# tabItem(tabName = "signUp",
#         h2("Can input missing Data")
# )
ui <- dashboardPage(header, sidebar, body, skin = "green")