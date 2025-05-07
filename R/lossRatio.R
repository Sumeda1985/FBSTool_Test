lossRatio <-function(input,output,session){

  observeEvent(input$undoLossratio, {
  # get last version
  new_version <- Pop_table_version("loss_ratio")  
  # nothing to reset -- optionally a warning could be displayed in this case
  if(is.null(new_version)) {
    return()
  }
   value$data_loss_ratio <- new_version
})

observeEvent(input$startContinue,{
  value$lossRatioLong <- data.table(dbReadTable(con, "loss_ratios"))
  value$data_loss_ratio <- wide_format(value$lossRatioLong)
  value$data_loss_ratio <- visualize_data(value$data_loss_ratio,input, session)
  value$data_loss_ratio[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  Add_table_version("loss_ratio", copy(value$data_loss_ratio)) 
})

observeEvent(input$add_Loss_ratio, {
  showModal(viewLossRatio())
})
viewLossRatio <- function(failed = FALSE) {
  modalDialog(
    easyClose = TRUE, size= "l",
    dataTableOutput("viewLossRatio"),
   footer = tagList(
      actionButton("LossRatioInsert", "Insert")
    )
  )
}
output$viewLossRatio= renderDataTable({
  commodity=copy(all_cpc)[order(CPCCode),]
  commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
  commodity <- commodity[!CPCCode %in% unique(value$data_loss_ratio$CPCCode)]
  value$lossratioCommodities <- commodity
  datatable(value$lossratioCommodities,escape=F)
})


observeEvent(input$LossRatioInsert, {
  removeModal()
})

observeEvent(input$loss_ratio_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('loss_ratio'),
    cell_edit_info = input$loss_ratio_cell_edit,
    data = value$data_loss_ratio,
    session = session,
    table_name = "loss_ratio"
  )
})
#insert loss ratio
observeEvent(input$LossRatioInsert, {
  handleInsertEvent(
    input_rows = input$viewLossRatio_rows_selected,
    element_codes = c("R5016"),
    value_store = value,
    data_type = "loss_ratio"
  )
  
})


#delete rows in crop table
observeEvent(input$delete_btn_loss_ratio, {
  # Create record of dropped data
  droplossRatiodata <- value$data_loss_ratio[
    as.numeric(input$loss_ratio_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("R5016"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, droplossRatiodata)
  value$data_loss_ratio <- value$data_loss_ratio[!(CPCCode %in% value$data_loss_ratio[
    as.numeric(input$loss_ratio_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("loss_ratio", copy(value$data_loss_ratio)) 
})


#download excle file 
output$downloadLossRatio  <- createDownloadHandler(reactive(value$data_loss_ratio), 
                                                   "lossRatio_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileLossRatiodenormalized", 
                         data_key = "data_loss_ratio", 
                         version_label =  "loss_ratio",
                         value_env = value)
observeEvent(input$uploadlossRatioModal, {
  showModal(uploadLossRatio())
})

uploadLossRatio <- function(failed = FALSE) {
  modalDialog(size = "l",
             titlePanel("Upload File"),
             sidebarLayout(
             sidebarPanel(
             fileInput("fileLossRatio", "Choose Excel File",
                            multiple = TRUE,
                            accept = NULL),
                  selectizeInput("cpcLossratio", "CPC Code",
                                 selected = NULL, choices = 
                                   c("",colnames( value$data_loss_ratioCountry)),multiple=F),
                  selectizeInput("elementLossratio", "Element Code",
                                 selected = NULL, choices = c("",colnames( value$data_loss_ratioCountry)) ,multiple=F),
                  selectizeInput("yearLossratio", "Year :",
                                 selected = NULL, choices = c("",colnames( value$data_loss_ratioCountry)),multiple=F),
                  selectizeInput("valueLossratio", "Value :",
                                 selected = NULL, choices = c("",colnames( value$data_loss_ratioCountry)),multiple=F),
                  selectizeInput("flagLossratio", "Flag :",
                                 selected = NULL, choices = c("",colnames( value$data_loss_ratioCountry)),multiple=F),
                  actionButton("uploadLossratio","Upload Loss ratio")
                  
                ),
                mainPanel(
                div(style = 'overflow-x: scroll', dataTableOutput('LossratioCountry'))
                )
                
              )
  )
  
}

output$LossratioCountry <- renderDataTable({
  req(input$fileLossRatio)
  inFile <- input$fileLossRatio
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  value$data_loss_ratioCountry <- DATA
  datatable(value$data_loss_ratioCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})

observe({
  updateSelectInput(session, "cpcLossratio", choices = c("", colnames( value$data_loss_ratioCountry)))
  updateSelectInput(session, "elementLossratio", choices = c("",colnames( value$data_loss_ratioCountry)))
  updateSelectInput(session, "yearLossratio", choices = c("",colnames( value$data_loss_ratioCountry)))
  updateSelectInput(session, "valueLossratio", choices = c("",colnames( value$data_loss_ratioCountry)))
  updateSelectInput(session, "flagLossratio", choices = c("",colnames( value$data_loss_ratioCountry)))
  
})

observeEvent(input$uploadLossratio, {
  normalized_upload(
    data_source = "data_loss_ratioCountry",
    input_cols = c("cpcLossratio", "elementLossratio", "yearLossratio", "valueLossratio", "flagLossratio"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("R5016"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_loss_ratio",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_loss_ratio",
    version_label = "loss_ratio",
    session = session
  )
})

observeEvent(input$uploadLossratio, {
  
  removeModal()
  
})
observeEvent(input$saveLossratio,{
  # Prepare data for saving
  data_to_save <- copy(value$data_loss_ratio)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("R5016")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$lossRatioLong ,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    table= tbl(con, "loss_ratios"),
    output = output,
    data_session = value$data_loss_ratio
  )
})

observeEvent(input$loss_imputation_ratio,{
  data <- imputationLossRatio(input,output,session)
  value$data_loss_ratio <- data
  Add_table_version("loss_ratio", copy(value$data_loss_ratio))
})
output$loss_ratio <-
  renderDataTable({
   datatable (value$data_loss_ratio, rownames= FALSE,class = 'cell-border stripe',
              editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_loss_ratio) 
                                                                                               == input$fromyear)-2)))),
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                scrollX = TRUE,
                 scrollY = "500px" ,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(list(width = '', targets =  c(6)),
                                 list(visible = FALSE, targets = (ncol(value$data_loss_ratio)-1))
                 )
               ))%>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
  } )

}