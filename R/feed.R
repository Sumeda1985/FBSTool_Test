feed <- function(input,output,session){
 observeEvent(input$undoFeed, {
   # get last version
   new_version <- Pop_table_version("feed")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
   value$data_feed <- new_version
})
  
  observeEvent(input$startContinue,{
    value$feedDataLong <- value_database$data[
      ElementCode %in% c("5520") &
        Year %in% c(2010:as.numeric(input$endyear)) &
        StatusFlag == 1,
      .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
    ][!is.na(Value)]
    feedData=value_database$data[ElementCode %in% c("5520") & StatusFlag == 1
                                 & Year %in% c(2010:input$endyear)][!is.na(Value)]
    feedData=wide_format(feedData)
    # Get column information
    yearcols <- grep("^[[:digit:]]{4}$", names(feedData), value = TRUE)
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
    if(input$endyear > maxyear + 1){
      yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
      value$data_feed <- NULL
      if(length(yearsToFill) > 0){
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please compile feed data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
          type = "error"
        )
      }
    } else {
      feedData = visualize_data(feedData,input, session)
      feedData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_feed <- feedData
      Add_table_version("feed", copy(value$data_feed))  
    }
  })  
  
observeEvent(input$add_Feed, {
    showModal(viewFeedTriplets())
})
  
viewFeedTriplets <- function(failed = FALSE) {
   modalDialog(
      easyClose = TRUE, size= "l",
      dataTableOutput("viewFeed"),
   footer = tagList(
     actionButton("feedInsert", "Insert"))
    )
    
}
  
output$viewFeed= renderDataTable({
    commodity=copy(all_cpc)[order(CPCCode),]
    commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
    commodity <- commodity[ !CPCCode %in% unique(value$data_feed$CPCCode)]
    value$feedCommodities <- commodity
    datatable(value$feedCommodities,escape=F)
})
  
 observeEvent(input$feedInsert, {
    removeModal()
 })
  
observeEvent(input$feed_cell_edit, {
   handle_cell_edit(
     proxy = dataTableProxy('feed'),
     cell_edit_info = input$feed_cell_edit,
     data = value$data_feed,
     session = session,
     table_name = "feed"
   )
 })
  
observeEvent(input$feedInsert, {
  handleInsertEvent(
    input_rows = input$viewFeed_rows_selected,
    element_codes = c("5520"),
    value_store = value,
    data_type = "feed"
  )   
})
  
#delete rows in feed table
observeEvent(input$delete_btn_feed, {
  # Create record of dropped data
  dropfeeddata <- value$data_feed[
    as.numeric(input$feed_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5525"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropfeeddata)
  value$data_feed <- value$data_feed[!(CPCCode %in% value$data_feed[
    as.numeric(input$feed_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("feed", copy(value$data_feed)) 
})
  
#download excle file 
output$downloadFeed <- createDownloadHandler(reactive(value$data_feed),"feed_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileFeeddenormalized", 
                         data_key = "data_feed", 
                         version_label =  "feed",
                         value_env = value)
observeEvent(input$uploadFeedModal, {
    showModal(uploadFeed())
  })
  
uploadFeed <- function(failed = FALSE) {
    modalDialog(size = "l",
                titlePanel("Upload File"),
                sidebarLayout(
                sidebarPanel(
                fileInput("fileFeed", "Choose Excel File", multiple = TRUE,
                          accept = NULL),
                selectizeInput("cpcFeed", "CPC Code",
                                   selected = NULL, choices = c("",colnames( value$data_feedCountry)),multiple=F),
                selectizeInput("elementFeed", "Element Code",
                                   selected = NULL, choices = c("",colnames( value$data_feedCountry)) ,multiple=F),
                selectizeInput("yearFeed", "Year :",
                                   selected = NULL, choices = c("",colnames( value$data_feedCountry)),multiple=F),
                selectizeInput("valueFeed", "Value :",
                                   selected = NULL, choices = c("",colnames( value$data_feedCountry)),multiple=F),
                selectizeInput("flagFeed", "Flag :",
                                   selected = NULL, choices = c("",colnames( value$data_feedCountry)),multiple=F),
                actionButton("uploadFeed","Upload Feed data")
               ),
                 mainPanel(
                   div(style = 'overflow-x: scroll', dataTableOutput('feedCountry'))
                  )
               )
    )
 }
  
output$feedCountry <- renderDataTable({
   req(input$fileFeed)
   inFile <- input$fileFeed
   file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
   DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
   value$data_feedCountry <- DATA
   datatable(value$data_feedCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
 })
  
 observe({
    updateSelectInput(session, "cpcFeed", choices = c("", colnames( value$data_feedCountry)))
    updateSelectInput(session, "elementFeed", choices = c("",colnames( value$data_feedCountry)))
    updateSelectInput(session, "yearFeed", choices = c("",colnames( value$data_feedCountry)))
    updateSelectInput(session, "valueFeed", choices = c("",colnames( value$data_feedCountry)))
    updateSelectInput(session, "flagFeed", choices = c("",colnames( value$data_feedCountry)))
    
  })
  
observeEvent(input$uploadFeed, {
   removeModal()
})
  
observeEvent(input$uploadFeed, {
 normalized_upload(
    data_source = "data_feedCountry",
    input_cols = c("cpcFeed", "elementFeed", "yearFeed", "valueFeed", "flagFeed"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("5520"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_feed",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_feed",
    version_label = "feed",
    session = session,
    input=input
  )
})
  
  
observeEvent(input$saveFeed, {
  # Prepare data for saving
  data_to_save <- copy(value$data_feed)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5520")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$feedDataLong ,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_feed
  )
})
  
observeEvent(input$feed_imputation,{
   data <- imputeFeed(input,output,session)
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
      value$data_feed <- data
      Add_table_version("feed", copy(value$data_feed))
}
})
  
output$feed <- 
    renderDataTable(
      if (!is.null(value$data_feed)){
        datatable (value$data_feed, rownames= FALSE,class = 'cell-border stripe', 
            editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_feed) == input$fromyear)-2)))),
                   extensions = c("FixedColumns","FixedHeader","Buttons"),
                   options = list(
                     pageLength = 25,
                     dom= 'Blfrtip', buttons = I('colvis'),
                     scrollX = TRUE,
                     scrollY = "500px" ,
                     autoWidth = T,
                     fixedColumns = list(leftColumns = 4),
                     columnDefs = list(
                     
                       list(visible = FALSE, targets = (ncol(value$data_feed)-1))
                     )
                   ))  %>%
          formatStyle(0:ncol(value$data_feed), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
  )
}