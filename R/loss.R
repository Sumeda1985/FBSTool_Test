
loss <- function(input,output,session){
  
observeEvent(input$undoLoss, {
# get last version
new_version <- Pop_table_version("loss")  
# nothing to reset -- optionally a warning could be displayed in this case
if(is.null(new_version)) {
      return()
    }
value$data_loss <- new_version
})
  
observeEvent(input$startContinue,{
 value$lossDataLong <- value_database$data[
 ElementCode %in% c("5016") &
 Year %in% c(2010:as.numeric(input$endyear)) &
 StatusFlag == 1,
  .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
 ][!is.na(Value)]
 lossData=value_database$data[ElementCode %in% c("5016") & StatusFlag == 1
                             & Year %in% c(2010:input$endyear)][!is.na(Value)]
 lossData=wide_format(lossData)
# Get column information
 yearcols <- grep("^[[:digit:]]{4}$", names(lossData), value = TRUE)
 minyear <- min(as.numeric(yearcols))
 maxyear <- max(as.numeric(yearcols))
 if(input$endyear > maxyear + 1){
  yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
  value$data_loss <- NULL
  if(length(yearsToFill) > 0){
    sendSweetAlert(
      session = session,
      title = "Error!!",
      text = paste("Please compile Loss data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
      type = "error"
    )
  }
} else {
  lossData = visualize_data(lossData,input, session)
  lossData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_loss <- lossData
  Add_table_version("loss", copy(value$data_loss))  
}
  })

observeEvent(input$add_Loss, {
  showModal(viewLossTriplets())
})
viewLossTriplets <- function(failed = FALSE) {
    modalDialog(
     easyClose = TRUE, size= "l",
     dataTableOutput("viewLoss"),
     footer = tagList(
        actionButton("lossInsert", "Insert")
      )
    )
}
output$viewLoss= renderDataTable({
    commodity=copy(all_cpc)[order(CPCCode),]
    commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
    value$lossCommodities  <- commodity[!CPCCode %in% unique(value$data_loss$CPCCode)]
    datatable(value$lossCommodities,escape=F)
 })
  
observeEvent(input$lossInsert, {
removeModal()
})
observeEvent(input$loss_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('loss'),
    cell_edit_info = input$loss_cell_edit,
    data = value$data_loss,
    session = session,
    table_name = "loss"
  )
})

observeEvent(input$lossInsert, {
  handleInsertEvent(
    input_rows = input$viewLoss_rows_selected,
    element_codes = c("5016"),
    value_store = value,
    data_type = "loss"
  )
})

#delete rows in crop table
observeEvent(input$delete_btn_loss, {
  # Create record of dropped data
  droplossdata <- value$data_loss[
    as.numeric(input$loss_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5016"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, droplossdata)
  value$data_loss <- value$data_loss[!(CPCCode %in% value$data_loss[
    as.numeric(input$loss_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("loss", copy(value$data_loss)) 
})

#download excle file 
output$downloadLoss  <- createDownloadHandler(reactive(value$data_loss), 
                                                    "loss_data.xlsx")
# #upload  denormalized data (#fileCrop)

upload_denormalized_data(input= input, 
                         session = session, 
                         value = list(data_loss = data.table(), data_feed = data.table()), 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileLossdenormalized", 
                         data_key = "data_loss", 
                         version_label =  "loss",
                         value_env = value)

observeEvent(input$uploadLossdModal, {
    showModal(uploadLoss())
})
  
uploadLoss <- function(failed = FALSE) {
   modalDialog(size = "l",
         titlePanel("Upload File"),
               # Sidebar layout with input and output definitions ----
                sidebarLayout(
                 # Sidebar panel for inputs ----
                sidebarPanel(
                # Input: Select a file ----
                fileInput("fileLoss", "Choose Excel File", multiple = TRUE,accept = NULL),
                # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                 selectizeInput("cpcLoss", "CPC Code",
                                   selected = NULL, choices = c("",colnames(value$data_lossCountry)),multiple=F),
                 selectizeInput("elementLoss", "Element Code",
                                   selected = NULL, choices = c("",colnames( value$data_lossCountry)) ,multiple=F),
                 selectizeInput("yearLoss", "Year :",
                                   selected = NULL, choices = c("",colnames( value$data_lossCountry)),multiple=F),
                 selectizeInput("valueLoss", "Value :",
                                   selected = NULL, choices = c("",colnames( value$data_lossCountry)),multiple=F),
                 selectizeInput("flagLoss", "Flag :",
                                   selected = NULL, choices = c("",colnames( value$data_lossCountry)),multiple=F),
                 actionButton("uploadLoss","Upload Loss data")
                  ),
                 # Main panel for displaying outputs ----
                  mainPanel(
                   div(style = 'overflow-x: scroll', dataTableOutput('lossCountry'))
                  )
                )
    )
 }
output$lossCountry <- renderDataTable({
 req(input$fileLoss)
 inFile <- input$fileLoss
 file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
 value$data_lossCountry=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
 datatable(value$data_lossCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})

observe({
 updateSelectInput(session, "cpcLoss", choices = c("", colnames( value$data_lossCountry)))
 updateSelectInput(session, "elementLoss", choices = c("",colnames( value$data_lossCountry)))
 updateSelectInput(session, "yearLoss", choices = c("",colnames( value$data_lossCountry)))
 updateSelectInput(session, "valueLoss", choices = c("",colnames( value$data_lossCountry)))
 updateSelectInput(session, "flagLoss", choices = c("",colnames( value$data_lossCountry)))
})

observeEvent(input$uploadLoss, {
  removeModal()
})
  
#############################################
observeEvent(input$uploadLoss, {
  normalized_upload(
    data_source = "data_lossCountry",
    input_cols = c("cpcLoss", "elementLoss", "yearLoss", "valueLoss", "flagLoss"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("5016"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_loss",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_loss",
    version_label = "loss",
    session = session,
    input = input
  )
})
observeEvent(input$saveLoss, {
  # Prepare data for saving
  data_to_save <- copy(value$data_loss)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5016")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$lossDataLong ,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_loss
  )
})
  observeEvent(input$loss_imputation,{
    data <- wide_format(imputeLoss(input,output,session))
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
      value$data_loss <- data
      Add_table_version("loss", copy(value$data_loss))
    }
 })
  
output$loss <- 
    renderDataTable(
     if (!is.null(value$data_loss)){
      datatable (value$data_loss, rownames= FALSE,class = 'cell-border stripe', 
       editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_loss) == input$fromyear)-2)))),
                   extensions = c("FixedColumns","FixedHeader","Buttons"),
                   options = list(
                     pageLength = 25,
                     scrollX = TRUE,
                     scrollY = "500px" ,
                     dom= 'Blfrtip', buttons = I('colvis'),
                     autoWidth = T,
                     fixedColumns = list(leftColumns = 4),
                     columnDefs = list(
                     list(visible = FALSE, targets = (ncol(value$data_loss)-1))
                     )
                   ))  %>%
         formatStyle(0:ncol(value$data_loss), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    }
)
}