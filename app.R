library(shiny)
source("global.R")
source("ui.R")
source("server.R")


shinyApp(ui, shinyServer)

## key <- hash(charToRaw("This is a secret passphrase"))
## msg <- serialize(iris, NULL)
## mytag <- data_tag(msg, key)

## library(sodium)
## key <- sig_keygen()
## pubkey <- sig_pubkey(key)
