
shinyServer(function(input, output, session) {


options(shiny.maxRequestSize=30*1024^2)

reactive({
updateSelectInput(session, input$countrym49, c(country_selc))
})
########
isolate(rv$active_sessions <- c(rv$active_sessions, session$token))
output$time <- renderText(rv$current_time)

onSessionEnded(fun = function(){
  isolate(rv$active_sessions <- setdiff(rv$active_sessions, session$token))
})

#reactive value for database
value_database <<- reactiveValues(data =NULL)

######################3
data_base <- observe({
  #reading country data
  countryData <- data.table(dbReadTable(con, "dbcountry"))[StatusFlag==1]
  ## countryData[, StatusFlag:=1]
  ## countryData[, LastModified:=as.numeric(Sys.time())]
  ## dbWriteTable(con, "dbcountry", countryData, overwrite=TRUE)
  countryData[, Value := as.numeric(Value)]
  countryData[, Flag := as.character(Flag)]
  # As per requested, live animals must be eliminated. Codes were provided by Giulia.
  live_animals <- c("02151", "02154", "02153", "02194", "02192.01","02191","02152",
                    "02132", "02112","02121.01","02111","02123","02131","02133","02121.02","02192","02122",
                    "02140")
  countryData <- countryData[!CPCCode %in% live_animals]
  #countrySUA data cpc codes must be changed with all_cpc
  countryData[, Commodity := NULL]
  countryData <- merge(countryData, all_cpc, by = "CPCCode", all.x = TRUE)
  setcolorder(countryData, c("CountryM49","Country","CPCCode","Commodity","ElementCode","Element","Year","Value"))
  value_database$data <- countryData

})

#crop reactive values
value <<- reactiveValues(data_crop =NULL, 
                         modifiedData = data.table(CPCCode=character(),
                                                   Commodity=character(),
                                                   Elementcode=character(),
                                                   Element=character(),
                                                   Year = numeric(),
                                                   Flag= character(),
                                                   Value = numeric(),
                                                   LastModified = numeric()))
df_cropCountry <<- reactiveValues(data_cropCountry =NULL)
valuesxxx <<-  reactiveValues(test = 'initial')
observeEvent(input$cropInsert,  {valuesxxx$test = 'add'})

#########
value <<- reactiveValues(countrym49 =NULL)

#livestock reactive values
value <<- reactiveValues(data_livestock =NULL)
df_livestockCountry <<- reactiveValues(data_livestockCountry =NULL)
valuesxxx <<-  reactiveValues(livestock_button = 'initial')
  # observeEvent(input$livestockInsert,  {valuesxxx$livestock_button = 'add'})
#domain scripts
crop_production <- crop_production(input,output,session)
livestock_production <- livestock_production(input,output,session)

observeEvent(input$startContinue,{
  newtab <- switch(input$fao,"Start" = "production")
  updateTabItems(session, "fao", newtab)
})

observeEvent(input$fromyear,{
if (input$fromyear != "" & nchar(input$fromyear) == 4){
    if(input$fromyear <= 2013){
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = "Please Select a year greater than 2013",
        type = "error"
      )
    }

  }
})

  observeEvent(input$endyear,{
if (input$endyear != "" & nchar(input$endyear) == 4){
  if(input$endyear <= 2013){
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Please Select a year greater than 2013",
        type = "error"
      )
    }
  }
})

observeEvent(c(input$fromyear, input$endyear),{
if (input$fromyear != "" & nchar(input$fromyear) == 4 & nchar(input$endyear) == 4 ){
   if(input$endyear < input$fromyear){
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = paste("Please Select a year greater than",input$fromyear),
        type = "error"
      )
    }

  }
})

saveMessages(input, output, session, buttons= c("saveCrop", "saveLivestock"))
session$onSessionEnded(function() {
  # #
  # # session$reload()
  # stopApp()
  # #
  # #
})
Sys.sleep(1)
})
