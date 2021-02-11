# run_api.R
library(plumber)
plumb(file = "api_endpoints.r")$run(port = 8000)