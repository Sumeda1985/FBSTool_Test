library(shiny)
source("global.R")
source("ui.R")
source("server.R")
userauth <- readRDS("userauth.rds")

shinyApp(ui, shinyServer)
