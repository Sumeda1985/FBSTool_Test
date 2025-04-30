
trade_imports <- function(input,output,session){
#### undo imports ####
 observeEvent(input$undoImports, {
    # get last version
    new_version <- Pop_table_version("imports")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    value$data_imports <- new_version
 })

observeEvent(input$startContinue,{
  value$importDataLong <- value_database$data[
 ElementCode %in% c("5610") &
      Year %in% c(2010:as.numeric(input$endyear)) &
      StatusFlag == 1,
    .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
  ][!is.na(Value)]
  
  # Get import data for wide format processing
  importData <- value_database$data[
    ElementCode %in% c("5610") &
      StatusFlag == 1 & Year %in% c(2010:input$endyear)
  ][!is.na(Value)]
 importData=wide_format(importData)
  # Get column information
yearcols <- grep("^[[:digit:]]{4}$", names(importData), value = TRUE)
 minyear <- min(as.numeric(yearcols))
 maxyear <- max(as.numeric(yearcols))
  
  if(input$endyear > maxyear + 1){
  yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
  value$data_imports <- NULL
    if(length(yearsToFill) > 0){
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Imports data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
}
  } else {
    importData = visualize_data(importData,input, session)
    importData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_imports <- importData
    Add_table_version("imports", copy(value$data_imports))  
  }
  
})

observeEvent(input$add_Imports, {
  showModal(viewImportsTriplets())
})

viewImportsTriplets <- function(failed = FALSE) {
modalDialog(
easyClose = TRUE, size= "l",
dataTableOutput("viewImports"),
footer = tagList(
actionButton("importsInsert", "Insert")
  )
)
  
}

output$viewImports= renderDataTable({
  commodity <- all_cpc[order(CPCCode),]
  commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
  value$importsCommodities <- commodity[!CPCCode %in% unique(value$data_imports$CPCCode)]
  datatable(value$importsCommodities,
            escape=F)
})

observeEvent(input$importsInsert, {
removeModal()
})

observeEvent(input$imports_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('imports'),
    cell_edit_info = input$imports_cell_edit,
    data = value$data_imports,
    session = session,
    table_name = "imports"
  )
})

observeEvent(input$importsInsert, {
  handleInsertEvent(
    input_rows = input$viewImports_rows_selected,
    element_codes = c("5610"),
    value_store = value,
    data_type = "imports"
  )
})

#delete rows 

observeEvent(input$delete_btn_imports, {
# Create record of dropped data
  dropimportdata <- value$data_imports[
    as.numeric(input$imports_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5610"),
      StatusFlag = 0
    )
  ]
 # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropimportdata)
  value$data_imports <- value$data_imports[!(CPCCode %in% value$data_imports[
    as.numeric(input$imports_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("imports", copy(value$data_imports)) 
})


#download excle file (#downloadCrop)
output$downloadImports <-createDownloadHandler(reactive(value$data_imports), 
                                               "import_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileImportsdenormalized", 
                         data_key = "data_imports", 
                         version_label =  "imports",
                         value_env = value)
observeEvent(input$uploadImportsModal, {
  showModal(uploadImports())
})
# 
uploadImports <- function(failed = FALSE) {
  modalDialog(
    size = "l",
    titlePanel("Upload File"),
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        fileInput("fileImports", "Choose Country Import Excel File (.xlsx)", multiple = TRUE, accept = NULL),
        selectizeInput("importHS", "HS Code (Please make sure that HS Code should contain at least 6 digits) :", 
                       selected = NULL, choices = c("", colnames(value$data_importsCountry)), multiple = FALSE),
        selectizeInput("importCommodity", "Commodity :", 
                       selected = NULL, choices = c("", colnames(value$data_importsCountry)), multiple = FALSE),
        selectizeInput("importQuantity", "Quantity (Please make sure that the quantity is in tons) :", 
                       selected = NULL, choices = c("", colnames(value$data_importsCountry)), multiple = FALSE),
        selectizeInput("importYear", "Year :", 
                       selected = NULL, choices = c("", colnames(value$data_importsCountry)), multiple = FALSE),
        actionButton("importMap", "Map Import Data with CPCCode")
      ),
      # Main panel for displaying outputs
      mainPanel(div(style = 'overflow-x: scroll', dataTableOutput('importCountry')))
    )
  )
}

observe({
  updateSelectInput(session, "importHS", choices = c("", colnames(value$data_importsCountry)))
  updateSelectInput(session, "importCommodity", choices = c("", colnames(value$data_importsCountry)))
  updateSelectInput(session, "importQuantity", choices = c("",colnames(value$data_importsCountry)))
  updateSelectInput(session, "importYear", choices = c("",colnames(value$data_importsCountry)))
})

observeEvent(input$importMap, {
removeModal()
})

output$importCountry <- renderDataTable({
req(input$fileImports)
inFile <- input$fileImports
file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
value$data_importsCountry <- DATA
datatable(value$data_importsCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})


observeEvent(input$importMap, {
  # Validate inputs
  if (any(c(input$importYear, input$importHS, input$importQuantity) == "")) {
    sendSweetAlert(session, "Warning !!", "Invalid data", "warning")
    return()
  }
  
  # Process file if provided
  if (!is.null(input$fileImports)) {
    sendSweetAlert(session, "Warning !!", 
                   paste("You are deleting existing import trade data of years", input$fromyear, "to", input$endyear),
                   "warning")
    
    # Process and update data
    value$data_imports <- tradeImportMapping(input, output, session)[!is.na(CPCCode)][
      , hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)
    ]
    Add_table_version("imports", copy(value$data_imports))
  }
})
  
###newly added for not mapped data
viewnotmappedImports <- function(failed = FALSE) {
   modalDialog(
     title = "The following HS codes were not mapped for Imports Trade Data",
     easyClose = TRUE, size= "l",
      dataTableOutput("notmapped_imports"),
      footer = tagList(downloadButton("notmappedimports_download", "Download Excel File"),
        modalButton("Dismiss")
      )
    )
}
observeEvent(input$importMap,{
  showModal(viewnotmappedImports())
})

  
output$notmapped_imports= renderDataTable({
   datatable(value$data_imports_not_mapped,escape=F)
 })
  
output$notmappedimports_download <- createDownloadHandler(reactive(value$data_imports_not_mapped), 
                                                          "import_data_not_mapped.xlsx")
  
  

############################################## 

#save crop
observeEvent(input$saveImports,{
  data_to_save <- copy(value$data_imports)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5610")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$importDataLong,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_imports
  )
})

output$imports <- 
  renderDataTable({
    if (!is.null(value$data_imports)) {
      datatable(
        value$data_imports,
        rownames = FALSE,
        class = 'cell-border stripe',
        editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which(colnames(value$data_imports) == input$fromyear)-2)))),
        extensions = c("FixedColumns", "FixedHeader", "Buttons"),
        options = list(
          pageLength = 25, dom = 'Blfrtip', buttons = I('colvis'),
          scrollX = TRUE, scrollY = "500px", autoWidth = TRUE,
          fixedColumns = list(leftColumns = 4),
          columnDefs = list(list(visible = FALSE, targets = (ncol(value$data_imports)-1)))
        )
      ) %>%
        formatStyle(0:ncol(value$data_imports), valueColumns = "hidden", `border-bottom` = styleEqual(1, "solid 3px")) %>%
        formatCurrency(as.character(c(2010:input$endyear)), currency = "", digits = 0, interval = 3, mark = ",")
    }
  })
    
}