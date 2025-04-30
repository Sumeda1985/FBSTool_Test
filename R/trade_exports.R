
trade_exports <- function(input,output,session){
#### undo exports ####
 observeEvent(input$undoExports, {
    # get last version
    new_version <- Pop_table_version("Exports")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    value$data_Exports <- new_version
 })

observeEvent(input$startContinue,{
  value$exportDataLong <- value_database$data[
 ElementCode %in% c("5910") &
      Year %in% c(2010:as.numeric(input$endyear)) &
      StatusFlag == 1,
    .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
  ][!is.na(Value)]
  
  # Get export data for wide format processing
  exportData <- value_database$data[
    ElementCode %in% c("5910") &
      StatusFlag == 1 & Year %in% c(2010:input$endyear)
  ][!is.na(Value)]
  exportData=wide_format(exportData)
  # Get column information
yearcols <- grep("^[[:digit:]]{4}$", names(exportData), value = TRUE)
 minyear <- min(as.numeric(yearcols))
 maxyear <- max(as.numeric(yearcols))
  
  if(input$endyear > maxyear + 1){
  yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
  value$data_Exports <- NULL
    if(length(yearsToFill) > 0){
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Exports data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
}
  } else {
    exportData = visualize_data(exportData,input, session)
    exportData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_Exports <- exportData
    Add_table_version("Exports", copy(value$data_Exports))  
  }
  
})

observeEvent(input$add_Exports, {
  showModal(viewExportsTriplets())
})

viewExportsTriplets <- function(failed = FALSE) {
modalDialog(
easyClose = TRUE, size= "l",
dataTableOutput("viewExports"),
footer = tagList(
actionButton("exportsInsert", "Insert")
  )
)
  
}

output$viewExports= renderDataTable({
  commodity <- all_cpc[order(CPCCode),]
  commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
  value$exportsCommodities <- commodity[!CPCCode %in% unique(value$data_Exports$CPCCode)]
  datatable(value$exportsCommodities,
            escape=F)
})

observeEvent(input$exportsInsert, {
removeModal()
})

observeEvent(input$Exports_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('Exports'),
    cell_edit_info = input$Exports_cell_edit,
    data = value$data_Exports,
    session = session,
    table_name = "Exports"
  )
})

observeEvent(input$exportsInsert, {
  handleInsertEvent(
    input_rows = input$viewExports_rows_selected,
    element_codes = c("5910"),
    value_store = value,
    data_type = "Exports"
  )
})

#delete rows 

observeEvent(input$delete_btn_exports, {
# Create record of dropped data
  dropexportdata <- value$data_Exports[
    as.numeric(input$Exports_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5910"),
      StatusFlag = 0
    )
  ]
 # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropexportdata)
  value$data_Exports <- value$data_Exports[!(CPCCode %in% value$data_Exports[
    as.numeric(input$Exports_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("exports", copy(value$data_Exports)) 
})
#download excle file (#downloadCrop)
output$downloadExports <- createDownloadHandler(reactive(value$data_Exports), 
                                                "export_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileExportsdenormalized", 
                         data_key = "data_Exports", 
                         version_label =  "Exports",
                         value_env = value)


observeEvent(input$uploadExportsModal, {
  showModal(uploadExports())
})
# 
uploadExports <- function(failed = FALSE) {
  modalDialog(
    size = "l",
    titlePanel("Upload File"),
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        fileInput("fileExports", "Choose Country Export Excel File (.xlsx)", multiple = TRUE, accept = NULL),
        selectizeInput("exportHS", "HS Code (Please make sure that HS Code should contain at least 6 digits) :", 
                       selected = NULL, choices = c("", colnames(value$data_exportsCountry)), multiple = FALSE),
        selectizeInput("exportCommodity", "Commodity :", 
                       selected = NULL, choices = c("", colnames(value$data_exportsCountry)), multiple = FALSE),
        selectizeInput("exportQuantity", "Quantity (Please make sure that the quantity is in tons) :", 
                       selected = NULL, choices = c("", colnames(value$data_exportsCountry)), multiple = FALSE),
        selectizeInput("exportYear", "Year :", 
                       selected = NULL, choices = c("", colnames(value$data_exportsCountry)), multiple = FALSE),
        actionButton("exportMap", "Map Export Data with CPCCode")
      ),
      # Main panel for displaying outputs
      mainPanel(div(style = 'overflow-x: scroll', dataTableOutput('exportCountry')))
    )
  )
}

observe({
  updateSelectInput(session, "exportHS", choices = c("", colnames(value$data_exportsCountry)))
  updateSelectInput(session, "exportCommodity", choices = c("", colnames(value$data_exportsCountry)))
  updateSelectInput(session, "exportQuantity", choices = c("",colnames(value$data_exportsCountry)))
  updateSelectInput(session, "exportYear", choices = c("",colnames(value$data_exportsCountry)))
})

observeEvent(input$exportMap, {
removeModal()
})

output$exportCountry <- renderDataTable({
req(input$fileExports)
inFile <- input$fileExports
file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
value$data_exportsCountry <- DATA
datatable(value$data_exportsCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})


observeEvent(input$exportMap, {
  # Validate inputs
  if (any(c(input$exportYear, input$exportHS, input$exportQuantity) == "")) {
    sendSweetAlert(session, "Warning !!", "Invalid data", "warning")
    return()
  }
  
  # Process file if provided
  if (!is.null(input$fileExports)) {
    sendSweetAlert(session, "Warning !!", 
                   paste("You are deleting existing export trade data of years", input$fromyear, "to", input$endyear),
                   "warning")
    
    # Process and update data
    value$data_Exports <- tradeExportMapping(input, output, session)[!is.na(CPCCode)][
      , hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)
    ]
    Add_table_version("Exports", copy(value$data_Exports))
  }
})
  
###newly added for not mapped data
viewnotmappedExports <- function(failed = FALSE) {
   modalDialog(
     title = "The following HS codes were not mapped for Exports Trade Data",
     easyClose = TRUE, size= "l",
      dataTableOutput("notmapped_exports"),
      footer = tagList(downloadButton("notmappedexports_download", "Download Excel File"),
        modalButton("Dismiss")
      )
    )
}
observeEvent(input$exportMap,{
  showModal(viewnotmappedExports())
})

  
output$notmapped_exports= renderDataTable({
   datatable(value$data_Exports_not_mapped,
              escape=F)
})
  
output$notmappedexports_download <- createDownloadHandler(reactive(value$data_Exports_not_mapped), 
                                                          "export_data_not_mapped.xlsx")
  


############################################## 

#save crop
observeEvent(input$saveExports,{
  data_to_save <- copy(value$data_Exports)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5910")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$exportDataLong,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_Exports
  )
})

output$Exports <- 
  renderDataTable({
    if (!is.null(value$data_Exports)) {
      datatable(
        value$data_Exports,
        rownames = FALSE,
        class = 'cell-border stripe',
        editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which(colnames(value$data_Exports) == input$fromyear)-2)))),
        extensions = c("FixedColumns", "FixedHeader", "Buttons"),
        options = list(
          pageLength = 25, dom = 'Blfrtip', buttons = I('colvis'),
          scrollX = TRUE, scrollY = "500px", autoWidth = TRUE,
          fixedColumns = list(leftColumns = 4),
          columnDefs = list(list(visible = FALSE, targets = (ncol(value$data_Exports)-1)))
        )
      ) %>%
        formatStyle(0:ncol(value$data_Exports), valueColumns = "hidden", `border-bottom` = styleEqual(1, "solid 3px")) %>%
        formatCurrency(as.character(c(2010:input$endyear)), currency = "", digits = 0, interval = 3, mark = ",")
    }
  })
    
}