library(rsconnect)


rsconnect::setAccountInfo(
   name   = Sys.getenv("account_name"),
   token  = Sys.getenv("shinyapps_token"),
   secret = Sys.getenv("shinyapps_secret")
)


deployApp()