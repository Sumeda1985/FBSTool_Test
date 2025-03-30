# Server function
shinyServer(function(input, output, session) {
  # Set maximum request size
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  # Update country selection
  reactive({
    updateSelectInput(session, input$countrym49, c(country_selc))
  })
  
  # Session management
  isolate(rv$active_sessions <- c(rv$active_sessions, session$token))
  output$time <- renderText(rv$current_time)
  
  onSessionEnded(fun = function() {
    isolate(rv$active_sessions <- setdiff(rv$active_sessions, session$token))
  })
  
  # Database reactive values
  value_database <<- reactiveValues(data = NULL)
  
  # Load and process country data
  observe({
    # Read country data from database
    countryData <- data.table(dbReadTable(con, "dbcountry"))[StatusFlag == 1]
    
    # Filter out live animals
    live_animals <- c(
      "02151", "02154", "02153", "02194", "02192.01", "02191", "02152",
      "02132", "02112", "02121.01", "02111", "02123", "02131", "02133",
      "02121.02", "02192", "02122", "02140"
    )
    
    # Process data
    countryData <- countryData[!CPCCode %in% live_animals][, Commodity := NULL]
    countryData <- merge(countryData, all_cpc, by = "CPCCode", all.x = TRUE)
    setcolorder(countryData, c(
      "CountryM49", "Country", "CPCCode", "Commodity",
      "ElementCode", "Element", "Year", "Value"
    ))
    
    value_database$data <- countryData
  })
  
  # Crop data reactive values
  value <<- reactiveValues(
    data_crop = NULL,
    modifiedData = data.table(
      CPCCode = character(),
      Commodity = character(),
      Elementcode = character(),
      Element = character(),
      Year = numeric(),
      Flag = character(),
      Value = numeric(),
      LastModified = numeric()
    ),
    dropcropdata = data.table(
      CountryM49 = character(),
      Country = character(),
      CPCCode = character(),
      ElementCode = character(),
      StatusFlag = numeric()
    ),
    insertcropdata = data.table(
      CountryM49 = character(),
      Country = character(),
      CPCCode = character(),
      ElementCode = character(),
      StatusFlag = numeric()
    )
  )
  
  # Additional reactive values
  df_cropCountry <<- reactiveValues(data_cropCountry = NULL)
  valuesxxx <<- reactiveValues(test = "initial")
  
  # Event handlers
  observeEvent(input$cropInsert, {
    valuesxxx$test <- "add"
  })
  
  value <<- reactiveValues(countrym49 = NULL)
  
  # Livestock data reactive values
  value <<- reactiveValues(data_livestock = NULL)
  df_livestockCountry <<- reactiveValues(data_livestockCountry = NULL)
  valuesxxx <<- reactiveValues(livestock_button = "initial")
  
  # Load domain scripts
  crop_production <- crop_production(input, output, session)
  # livestock_production <- livestock_production(input, output, session)
  
  # Navigation handlers
  observeEvent(input$startContinue, {
    newtab <- switch(input$fao, "Start" = "production")
    updateTabItems(session, "fao", newtab)
  })
  
  # Year validation handlers
  observeEvent(input$fromyear, {
    if (input$fromyear != "" && nchar(input$fromyear) == 4) {
      if (input$fromyear <= 2013) {
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = "Please Select a year greater than 2013",
          type = "error"
        )
      }
    }
  })
  
  observeEvent(input$endyear, {
    if (input$endyear != "" && nchar(input$endyear) == 4) {
      if (input$endyear <= 2013) {
        sendSweetAlert(
          session = session,
          title = "Error !!",
          text = "Please Select a year greater than 2013",
          type = "error"
        )
      }
    }
  })
  
  observeEvent(c(input$fromyear, input$endyear), {
    if (input$fromyear != "" && nchar(input$fromyear) == 4 && 
        nchar(input$endyear) == 4) {
      if (input$endyear < input$fromyear) {
        sendSweetAlert(
          session = session,
          title = "Error !!",
          text = paste("Please Select a year greater than", input$fromyear),
          type = "error"
        )
      }
    }
  })
  
  # Save message handlers
  saveMessages(input, output, session, buttons = c("saveCrop", "saveLivestock"))
  
  # Session cleanup
  session$onSessionEnded(function() {
    # Add any cleanup code here if needed
  })
  
  Sys.sleep(1)
})
