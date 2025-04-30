# Server function
shinyServer(function(input, output, session) {
  # Set maximum request size
  options(shiny.maxRequestSize = 30 * 1024^2)
  # Update country selection
  reactive({
    updateSelectInput(session, input$countrym49, c(country_selc))
  })
  # Save message handlers
  saveMessages(input, output, session, buttons = c("saveCrop", "saveLivestock",
                            "saveImports","saveExports","saveStock", "saveLoss", "saveFeed",
                            "saveIndustry", "saveFood","saveLossratio",
                            "saveFeedratio","saveSeedratio","treeSave","nutrientSave","saveFish"))
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
      "ElementCode", "Element", "Year", "Value", "Flag" ,"StatusFlag","LastModified"
    ))
    value_database$data <- countryData
})
  
  #  data reactive values
  value <<- reactiveValues(
    data_save = NULL,
    modifiedData = data.table(
      CPCCode = character(),
      Commodity = character(),
      Elementcode = character(),
      Element = character(),
      Year = character(),
      Flag = character(),
      Value = numeric(),
      LastModified = numeric()
    ),
    dropdata = data.table(
      CountryM49 = character(),
      Country = character(),
      CPCCode = character(),
      ElementCode = character(),
      StatusFlag = numeric()
    ),
    insertdata = data.table(
      CountryM49 = character(),
      Country = character(),
      CPCCode = character(),
      ElementCode = character(),
      Year = character(),
      Flag = character(),
      Value = numeric(),
      StatusFlag = numeric()
    )
  )
  observeEvent(input$gotosuaBalanced,{
    newtab <- switch(input$fao,
                     "sua_unbalanced" = "sua_balanced"
    )
    updateTabItems(session, "fao", newtab)
  })
  value <<- reactiveValues(countrym49 = NULL)
  # Livestock data reactive values
  value <<- reactiveValues(data_livestock = NULL)
  df_livestockCountry <<- reactiveValues(data_livestockCountry = NULL)
  valuesxxx <<- reactiveValues(livestock_button = "initial")
  # Load domain scripts
  crop_production <- crop_production(input, output, session)
  livestock_production <- livestock_production(input, output, session)
  trade_imports <- trade_imports(input,output,session)
  trade_exports <- trade_exports(input,output,session)
  stock_change <- stock_change(input,output,session)
  loss <- loss(input,output,session)
  lossRatio<- lossRatio(input,output,session)
  feed <- feed(input,output,session)
  feedRatio <- feedRatio(input,output,session)
  seed <- seed(input,output,session)
  seedRates <- seedRates(input,output,session)
  industry <- industry(input,output,session)
  food <- food(input,output,session)
  SUA_Unbalanced <- SUA_Unbalanced(input,output,session)
  SUA_Balanced <- SUA_Balanced(input,output,session)
  FBS_Balanced <- FBS_Balanced(input,output,session)
  fbs_report <- fbs_report(input,output,session)
  GIFT <- GIFT(input,output,session)
  generate_self_sufficiency_tab(input,output,session)
  reference_tables <- reference_tables(input,output,session)
  fish <- fish(input,output,session)
 # Navigation handlers
  observeEvent(input$startContinue, {
    newtab <- switch(input$fao, "Start" = "production")
    updateTabItems(session, "fao", newtab)
  })
  observeEvent(input$fbs_balanced_button,{
   session$sendCustomMessage("set_active_tab", c(11, "#shiny-tab-fbs"))
})
observeEvent(input$fbs_report_button,{
    newtab <- switch(input$fao,"fbs" = "fbs_report")
    updateTabItems(session, "fao", newtab)
})
observeEvent(input$total_DES,{
 session$sendCustomMessage("set_active_tab", c(12, "#shiny-tab-total_des"))
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

#tree and nutrient data for a new year 
observeEvent(input$sua_unbalanced,{
    t=as.character(input$endyear)
    #after validating the tree write to the folder
    tree <- fread_rds("SUA-FBS Balancing/Data/tree.rds")
    years_missing = c()
    treeToadd = list()
    for (i in c(2010:t)){
      condition <- i %in% unique(tree$timePointYears)
      if (condition == FALSE){
        years_missing<- c(years_missing,i)
      }
   }
    if (length(years_missing) != 0){
      for (i in 1:length(years_missing)){
        tree_new <- tree[timePointYears == as.numeric(input$endyear) - 1]
        treeToadd_ <- copy(tree_new)[,timePointYears := years_missing[i]]
        treeToadd[[i]] <- treeToadd_
    }
 }
    treeToadd <- rbindlist(treeToadd)
    tree <- rbind(tree,treeToadd)
    tree[!duplicated(tree)]
    saveRDS(tree,"SUA-FBS Balancing/Data/tree.rds")
    ##################Nutrient Data ################################
    nutrientData <- fread_rds("SUA-FBS Balancing/Data/nutrientData.rds")
    years_missing_nutrient = c()
    nutrientToadd = list()
    for (i in c(2010:t)){
      condition_nutrient <- i %in% unique(nutrientData$timePointYears)
      if (condition_nutrient == FALSE){
       years_missing_nutrient<- c(years_missing_nutrient,i)
   }
 }
    if (length(years_missing_nutrient) != 0){
      for (i in 1:length(years_missing_nutrient)){
        nutrient_new<- nutrientData[timePointYearsSP == as.numeric(input$endyear)-1]
        nutrientToadd_ <- copy(nutrient_new)[,timePointYearsSP := years_missing_nutrient[i]]
        nutrientToadd[[i]] <- nutrientToadd_
      }
 }
     nutrientToadd <- rbindlist(nutrientToadd)
     nutrientData <- rbind(nutrientData,nutrientToadd)
     nutrientData[!duplicated(nutrientData)]
     saveRDS(nutrientData,"SUA-FBS Balancing/Data/nutrientData.rds")
  })
# Session cleanup
  session$onSessionEnded(function() {
    # Add any cleanup code here if needed
  })
  
  Sys.sleep(1)
})
