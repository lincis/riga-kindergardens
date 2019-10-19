library(rsconnect)

# Print a list of app dependencies. Libraries need to be loaded
# before publishing so deployApp() knows what is necessary.
source("./R/util.R")

source(here::here("load-data.R"))

# Set the account info for deployment.
setAccountInfo(
  name   = Sys.getenv("SHINY_NAME")
  , token  = Sys.getenv("SHINY_TOKEN")
  , secret = Sys.getenv("SHINY_SECRET"))

# Deploy the application.
deployApp()