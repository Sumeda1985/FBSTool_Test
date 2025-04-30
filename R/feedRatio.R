feedRatio <- function(input,output,session){

observeEvent(input$undoFeedratio, {
 # get last version
  new_version <- Pop_table_version("feed_ratio")  
  # nothing to reset -- optionally a warning could be displayed in this case
  if(is.null(new_version)) {
    return()
  }
  value$data_feed_ratio <- new_version
})
observeEvent(input$startContinue,{
  feed_ratios<- fread_rds("Data/feedRatio.rds")
  feed_ratios <- visualize_data(feed_ratios,input, session)
  feed_ratios[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_feed_ratio <- feed_ratios
  Add_table_version("feed_ratio", copy(value$data_feed_ratio))  
})
observeEvent(input$add_Feed_ratio, {
  showModal(viewFeedRatio())
})

viewFeedRatio <- function(failed = FALSE) {
  modalDialog(
    easyClose = TRUE, size= "l",
    dataTableOutput("viewFeedRatio"),
    footer = tagList(
      actionButton("FeedRatioInsert", "Insert")
    )
  )
  
}
output$viewFeedRatio= renderDataTable({
  commodity=copy(all_cpc)[order(CPCCode),]
   commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
  commodity <- commodity[!CPCCode %in% unique(value$data_feed_ratio$CPCCode)]
  value$feedratioCommodities <- commodity
  datatable(value$feedratioCommodities, escape=F)
})

observeEvent(input$FeedRatioInsert, {
  removeModal()
})

observeEvent(input$feed_ratio_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('feed_ratio'),
    cell_edit_info = input$feed_ratio_cell_edit,
    data = value$data_feed_ratio,
    session = session,
    table_name = "feed_ratio"
  )
})

observeEvent(input$FeedRatioInsert, {
  handleInsertEvent(
    input_rows = input$viewFeedRatio_rows_selected,
    element_codes = c("R5520"),
    value_store = value,
    data_type = "feed_ratio"
  )   
})
#delete rows in crop table

observeEvent(input$delete_btn_feed_ratio, {
  t = copy(value$data_feed_ratio)
  if (!is.null(input$feed_ratio_rows_selected)) {
    t <- t[-as.numeric(input$feed_ratio_rows_selected),]
  }
  value$data_feed_ratio<- t
  Add_table_version("feed_ratio", copy(value$data_feed_ratio))  
})

#download excle file 
output$downloadFeedRatio <- createDownloadHandler(reactive(value$data_feed_ratio),"feed_ratios.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileFeedRatiodenormalized", 
                         data_key = "data_feed_ratio", 
                         version_label =  "feed_ratio",
                         value_env = value)

observeEvent(input$uploadFeedRatioModal, {
  showModal(uploadFeedRatio())
})

uploadFeedRatio <- function(failed = FALSE) {
  modalDialog(size = "l",
         titlePanel("Upload File"),
         sidebarLayout(
         sidebarPanel(
         fileInput("fileFeedRatio", "Choose Excel File",
                            multiple = TRUE,
                            accept = NULL),
          selectizeInput("cpcFeedratio", "CPC Code",selected = NULL, choices = 
                           c("",colnames( value$data_feed_ratioCountry)),multiple=F),
          selectizeInput("elementFeedratio", "Element Code",selected = NULL, choices = 
                           c("",colnames( value$data_feed_ratioCountry)) ,multiple=F),
          selectizeInput("yearFeedratio", "Year :",selected = NULL, choices = 
                           c("",colnames( value$data_feed_ratioCountry)),multiple=F),
          selectizeInput("valueFeedratio", "Value :",selected = NULL, choices = 
                           c("",colnames( value$data_feed_ratioCountry)),multiple=F),
          selectizeInput("flagFeedratio", "Flag :", selected = NULL, choices = 
                           c("",colnames( value$data_feed_ratioCountry)),multiple=F),
          actionButton("uploadFeedratio","Upload Feed ratio")
              ),
                
            mainPanel(div(style = 'overflow-x: scroll', dataTableOutput('FeedratioCountry')))
              )
  )
}

output$FeedratioCountry <- renderDataTable({
  req(input$fileFeedRatio)
  inFile <- input$fileFeedRatio
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  value$data_feed_ratioCountry <- DATA
  datatable(value$data_feed_ratioCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
 })

observe({
  updateSelectInput(session, "cpcFeedratio", choices = c("", colnames( value$data_feed_ratioCountry)))
  updateSelectInput(session, "elementFeedratio", choices = c("",colnames( value$data_feed_ratioCountry)))
  updateSelectInput(session, "yearFeedratio", choices = c("",colnames( value$data_feed_ratioCountry)))
  updateSelectInput(session, "valueFeedratio", choices = c("",colnames( value$data_feed_ratioCountry)))
  updateSelectInput(session, "flagFeedratio", choices = c("",colnames( value$data_feed_ratioCountry)))
})

observeEvent(input$uploadFeedratio, {
  removeModal()
})

observeEvent(input$uploadFeedratio, {
  normalized_upload(
    data_source = "data_feed_ratioCountry",
    input_cols = c("cpcFeedratio", "elementFeedratio", "yearFeedratio", "valueFeedratio", "flagFeedratio"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("R5520"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_feed_ratio",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_feed_ratio",
    version_label = "feed_ratio",
    session = session
  )
})

observeEvent(input$feed_imputation_ratio,{
  data <- imputationFeedRatio(input,output,session)
  value$data_feed_ratio <- data
  Add_table_version("feed_ratio", copy(value$data_feed_ratio))
})

observeEvent(input$saveFeedratio,{
  data_to_save <- value$data_feed_ratio
  saveRDS(data_to_save,"Data/feedRatio.rds")
})
output$feed_ratio <-
  renderDataTable({
    datatable (value$data_feed_ratio, rownames= FALSE,class = 'cell-border stripe',
               editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_feed_ratio) 
                                                                                               == input$fromyear)-2)))),
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                scrollX = TRUE,
                 scrollY = "500px" ,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(list(width = '', targets =  c(6)),
                      list(visible = FALSE, targets = (ncol(value$data_feed_ratio)-1))
                 )
               ))%>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    
  } )
}
