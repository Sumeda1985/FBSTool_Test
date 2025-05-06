library(shiny)
library(RSQLite)
library(DBI)
library(data.table)
library(readxl)
library(sodium)

cmd <- 'ssh::ssh_tunnel(ssh::ssh_connect(host = "vikasguest@badal.sser.in:22", passwd="ERpWPM0JhN"), port = 6666, target = "127.0.0.1:5432")'
pid <- sys::r_background(
                std_out = FALSE,
                std_err = FALSE,
                args = c("-e", cmd)
            )


con <- dbConnect(RPostgres::Postgres(), dbname = "suafbsdb",
                 host = '127.0.0.1',
                 port = 6666,
                 user = 'suafbsdbuser',
                 pass = 'xeoEJ7UOxiQQ') #for public data (user is able to change data)

cmd2 <- 'ssh::ssh_tunnel(ssh::ssh_connect(host = "vikasguest@badal.sser.in:22", passwd="ERpWPM0JhN"), port = 7777, target = "127.0.0.1:5432")'

pid2 <- sys::r_background(
                 std_out = FALSE,
                 std_err = FALSE,
                 args = c("-e", cmd2)
             )

concore <- dbConnect(RPostgres::Postgres(), dbname = "suafbsdb",
                     host = '127.0.0.1',
                     port = 7777,
                     user = 'suafbsdbuser',
                     pass = 'xeoEJ7UOxiQQ',
                     options = "-c search_path=core")
## static data. User is not able to change data.


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
