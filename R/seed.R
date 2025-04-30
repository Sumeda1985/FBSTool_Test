seed <- function(input,output,session){
  
observeEvent(input$undoSeed, {
 # get last version
    new_version <- Pop_table_version("seed")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
   value$data_seed <- new_version
})
  
observeEvent(input$startContinue,{
    value$seedDataLong <- value_database$data[
      ElementCode %in% c("5525","5025" ) &
        Year %in% c(2010:as.numeric(input$endyear)) &
        StatusFlag == 1,
      .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
    ][!is.na(Value)]
    seedData=value_database$data[ElementCode %in% c("5525","5025") & StatusFlag == 1
                                 & Year %in% c(2010:input$endyear)][!is.na(Value)]
    seedData=wide_format(seedData)
    # Get column information
    yearcols <- grep("^[[:digit:]]{4}$", names(seedData), value = TRUE)
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
    if(input$endyear > maxyear + 1){
      yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
      value$data_seed <- NULL
      if(length(yearsToFill) > 0){
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please compile seed data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
          type = "error"
        )
      }
    } else {
      seedData = visualize_data(seedData,input, session)
      seedData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_seed <- seedData
      Add_table_version("seed", copy(value$data_seed))  
    }
  }) 
  
 observeEvent(input$add_Seed, {
    showModal(viewSeedTriplets())
 })
  
viewSeedTriplets <- function(failed = FALSE) {
  modalDialog(
      easyClose = TRUE, size= "l",
      dataTableOutput("viewSeed"),
      footer = tagList(
        actionButton("seedInsert", "Insert")
      )
    )
}

observeEvent(input$seedInsert, {
  handleInsertEvent(
    input_rows = input$viewSeed_rows_selected,
    element_codes = c("5525", "5025"),
    value_store = value,
    data_type = "seed"
  )   
})
  
output$viewSeed= renderDataTable({
    commodity=copy(all_cpc)[order(CPCCode),]
    commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
    commodity <- commodity[!CPCCode %in% unique(value$data_seed$CPCCode)]
    value$seedCommodities <- commodity
    datatable(value$seedCommodities,
              escape=F)
})
  
observeEvent(input$seedInsert, {
  removeModal()
 })
  
observeEvent(input$seed_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('seed'),
    cell_edit_info = input$seed_cell_edit,
    data = value$data_seed,
    session = session,
    table_name = "seed"
  )
})
  

#delete rows in seed table
observeEvent(input$delete_btn_seed, {
  # Create record of dropped data
  dropseeddata <- value$data_seed[
    as.numeric(input$seed_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5525"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropseeddata)
  value$data_seed <- value$data_seed[!(CPCCode %in% value$data_seed[
    as.numeric(input$seed_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("seed", copy(value$data_seed)) 
})
  
#download excle file 
output$downloadSeed <- createDownloadHandler(reactive(value$data_seed),"seed_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileSeeddenormalized", 
                         data_key = "data_seed", 
                         version_label =  "seed",
                         value_env = value)
observeEvent(input$uploadSeedModal, {
    showModal(uploadSeed())
})
  
uploadSeed <- function(failed = FALSE) {
    modalDialog(size = "l",
             titlePanel("Upload File"),
             sidebarLayout(
             sidebarPanel(
                fileInput("fileSeed", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
                selectizeInput("cpcSeed", "CPC Code",
                                   selected = NULL, choices = c("",colnames( value$data_seedCountry)),multiple=F),
                selectizeInput("elementSeed", "Element Code",
                                   selected = NULL, choices = c("",colnames( value$data_seedCountry)) ,multiple=F),
                selectizeInput("yearSeed", "Year :",
                                   selected = NULL, choices = c("",colnames( value$data_seedCountry)),multiple=F),
                selectizeInput("valueSeed", "Value :",
                                   selected = NULL, choices = c("",colnames( value$data_seedCountry)),multiple=F),
                selectizeInput("flagSeed", "Flag :",
                                   selected = NULL, choices = c("",colnames( value$data_seedCountry)),multiple=F),
                actionButton("uploadSeed","Upload Seed data")
                  ),
                  mainPanel(
                   div(style = 'overflow-x: scroll', dataTableOutput('seedCountry'))
                  )
                )
    )
 }
  
 observeEvent(input$uploadSeed, {
   removeModal()
 })
 
output$seedCountry <- renderDataTable({
    req(input$fileSeed)
    inFile <- input$fileSeed
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    value$data_seedCountry <- DATA
    datatable(value$data_seedCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})
  
observe({
    updateSelectInput(session, "cpcSeed", choices = c("", colnames( value$data_seedCountry)))
    updateSelectInput(session, "elementSeed", choices = c("",colnames( value$data_seedCountry)))
    updateSelectInput(session, "yearSeed", choices = c("",colnames( value$data_seedCountry)))
    updateSelectInput(session, "valueSeed", choices = c("",colnames( value$data_seedCountry)))
    updateSelectInput(session, "flagSeed", choices = c("",colnames( value$data_seedCountry)))
})
  
observeEvent(input$uploadSeed, {
  normalized_upload(
    data_source = "data_seedCountry",
    input_cols = c("cpcSeed", "elementSeed", "yearSeed", "valueSeed", "flagSeed"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("5525"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_seed",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_seed",
    version_label = "seed",
    session = session,
    input=input
  )
})
  
observeEvent(input$saveSeed, {
  # Prepare data for saving
  data_to_save <- copy(value$data_seed)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5525", "5025")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$seedDataLong ,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_seed
  )
})  
  
observeEvent(input$seed_imputation,{
    data <- imputeSeed(input,output,session)
    data <- wide_format(data)
    if ( is.null(data) ){
      sendSweetAlert(
        session = session,
        title = c("Imputation Error"),
        text = c("No time series data to impute"),
        type = "warning"
      )
      
    } else {
      data=visualize_data(data,input,session)
      data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_seed <- data
      Add_table_version("seed", copy(value$data_seed))
 }
})
  
output$seed <- 
    renderDataTable(
      if (!is.null(value$data_seed)){
        datatable (value$data_seed, rownames= FALSE,class = 'cell-border stripe', 
            editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_seed) == input$fromyear)-2)))),
                   extensions = c("FixedColumns","FixedHeader","Buttons"),
                   options = list(
                     pageLength = 25,
                     dom= 'Blfrtip', buttons = I('colvis'),
                     scrollX = TRUE,
                     scrollY = "500px" ,
                     autoWidth = T,
                     fixedColumns = list(leftColumns = 4),
                     columnDefs = list(
                     
                       list(visible = FALSE, targets = (ncol(value$data_seed)-1))
                     )
                   ))  %>%
          formatStyle(0:ncol(value$data_seed), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
    )
}