## Custom login page
loginpage <- div(
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
            
            shinyjs::hidden(
                div(
                    id = "nomatch",
                    tags$p(
                        "Oops! Incorrect username or password!",
                        style = "color: red; font-weight: 600;
                                        padding-top: 5px;font-size:16px;",
                        class = "text-center"
                    )
                )
            ),
            
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