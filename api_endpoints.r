# Ref: https://www.statworx.com/en/blog/how-to-create-rest-apis-with-r-plumber/
library(plumber)
library(jsonlite)


#* @apiTitle ping test
#*
#* @param req
#* @get /ping
function() {
    return ("ping text successful!")
}

#* @apiTitle Plumber Example API
#* 
#* Serve iris data
#* @param sp Species
#* @get /iris
function(sp) {
  df = iris[iris$Species == sp, -5]
  return (df)
}

#* @apiTitle Plumber Example API
#* 
#* Serve iris data
#* @param accuracy accuracy score of a model
#* @get /get_accuracy
function() {
  acc = runif(n = 1, min = 0.3, max = 0.7)
  return (acc)
}