library(googleAuthR)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/urlshortener"))

shorten_url <- function(url){
    
    body = list(
        longUrl = url
    )
    
    f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                           "POST",
                           data_parse_function = function(x) x$id)
    
    f(the_body = body)
    
}
