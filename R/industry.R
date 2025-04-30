
industry <- function(input,output,session){
  observeEvent(input$undoIndustry, {
    # get last version
    new_version <- Pop_table_version("industry")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    value$data_industry <- new_version
 })
  
  observeEvent(input$startContinue,{
    value$industryDataLong <- value_database$data[
      ElementCode %in% c("5165") &
        Year %in% c(2010:as.numeric(input$endyear)) &
        StatusFlag == 1,
      .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
    ][!is.na(Value)]
    industryData=value_database$data[ElementCode %in% c("5165") & StatusFlag == 1
                                     & Year %in% c(2010:input$endyear)][!is.na(Value)]
    industryData=wide_format(industryData)
    # Get column information
    yearcols <- grep("^[[:digit:]]{4}$", names(industryData), value = TRUE)
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
    if(input$endyear > maxyear + 1){
      yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
      value$data_industry <- NULL
      if(length(yearsToFill) > 0){
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please compile industry data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
          type = "error"
        )
      }
    } else {
      industryData = visualize_data(industryData,input, session)
      industryData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_industry <- industryData
      Add_table_version("industry", copy(value$data_industry))  
    }
  }) 
  
observeEvent(input$add_Industry, {
    showModal(viewIndustryTriplets())
  })
viewIndustryTriplets <- function(failed = FALSE) {
    modalDialog(
      easyClose = TRUE, size= "l",
      dataTableOutput("viewIndustry"),
      footer = tagList(
      actionButton("industryInsert", "Insert")
      )
    )
}
  
output$viewIndustry= renderDataTable({
    commodity=copy(all_cpc)[order(CPCCode),]
    commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
    commodity <- commodity[!CPCCode %in% unique(value$data_industry$CPCCode)]
    value$industryCommodities <- commodity
    datatable(value$industryCommodities,escape=F)
})
  
observeEvent(input$industryInsert, {
    removeModal()
})
  
observeEvent(input$industry_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('industry'),
    cell_edit_info = input$industry_cell_edit,
    data = value$data_industry,
    session = session,
    table_name = "industry"
  )
})  
  
observeEvent(input$industryInsert, {
  handleInsertEvent(
    input_rows = input$viewIndustry_rows_selected,
    element_codes = c("5165"),
    value_store = value,
    data_type = "industry"
  )    
})
  
#delete rows in seed table
observeEvent(input$delete_btn_industry, {
  # Create record of dropped data
  dropindustrydata <- value$data_industry[
    as.numeric(input$industry_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5165"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropindustrydata)
  value$data_industry <- value$data_industry[!(CPCCode %in% value$data_industry[
    as.numeric(input$industry_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("industry", copy(value$data_industry)) 
}) 
#download excle file (#downloadCrop)
output$downloadIndustry<-  createDownloadHandler(reactive(value$data_industry),"industry_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileIndustrydenormalized", 
                         data_key = "data_industry", 
                         version_label =  "industry",
                         value_env = value)  
observeEvent(input$uploadIndustryModal, {
  showModal(uploadIndustry())
})
observeEvent(input$uploadIndustry, {
  removeModal()
})
uploadIndustry <- function(failed = FALSE) {
   modalDialog(size = "l",
               titlePanel("Upload File"),
                sidebarLayout(
                sidebarPanel(
                fileInput("fileIndustry", "Choose Excel File", multiple = TRUE,
                              accept = NULL),
               selectizeInput("cpcIndustry", "CPC Code",
                                   selected = NULL, choices = c("",colnames( value$data_industryCountry)),multiple=F),
               selectizeInput("elementIndustry", "Element Code",
                                   selected = NULL, choices = c("",colnames( value$data_industryCountry)) ,multiple=F),
               selectizeInput("yearIndustry", "Year :",
                                   selected = NULL, choices = c("",colnames( value$data_industryCountry)),multiple=F),
               selectizeInput("valueIndustry", "Value :",
                                   selected = NULL, choices = c("",colnames( value$data_industryCountry)),multiple=F),
               selectizeInput("flagIndustry", "Flag :",
                                   selected = NULL, choices = c("",colnames( value$data_industryCountry)),multiple=F),
               actionButton("uploadIndustry","Upload Industry data")
                 ),
                  
               mainPanel(
                div(style = 'overflow-x: scroll', dataTableOutput('industryCountry')) )
               )
         )
  }
  
 output$industryCountry <- renderDataTable({
    req(input$fileIndustry)
    inFile <- input$fileIndustry
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    value$data_industryCountry <- DATA
    datatable(value$data_industryCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
 })
  
 observe({
   updateSelectInput(session, "cpcIndustry", choices = c("", colnames( value$data_industryCountry)))
    updateSelectInput(session, "elementIndustry", choices = c("",colnames( value$data_industryCountry)))
    updateSelectInput(session, "yearIndustry", choices = c("",colnames( value$data_industryCountry)))
    updateSelectInput(session, "valueIndustry", choices = c("",colnames( value$data_industryCountry)))
    updateSelectInput(session, "flagIndustry", choices = c("",colnames( value$data_industryCountry)))
 })
  
 observeEvent(input$uploadIndustry, {
   normalized_upload(
     data_source = "data_industryCountry",
     input_cols = c("cpcIndustry", "elementIndustry", "yearIndustry", "valueIndustry", "flagIndustry"),
     std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
     filter_element_code = c("5165"),
     from_year = input$fromyear,
     to_year = input$endyear,
     existing_data = "data_industry",
     all_elements = all_elements,
     all_cpc = all_cpc,
     value_env = value,
     value_name = "data_industry",
     version_label = "industry",
     session = session,
     input=input
   )
 })

 observeEvent(input$saveIndustry, {
   # Prepare data for saving
   data_to_save <- copy(value$data_industry)
   data_to_save[, hidden := NULL]
   data_to_save <- data_to_save[ElementCode %in% c("5165")]
   # Save to database
   save_to_database(
     data = data_to_save,
     longData = value$industryDataLong ,
     year_range = c(input$fromyear:input$endyear),
     session = session,
     input = input,
     output = output,
     data_session = value$data_industry
   )
 })  
 
output$industry <- 
    renderDataTable(
      if (!is.null(value$data_industry)){
        datatable (value$data_industry, rownames= FALSE,class = 'cell-border stripe', 
           editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_industry) == input$fromyear)-2)))),
                   extensions = c("FixedColumns","FixedHeader","Buttons"),
                   options = list(
                     pageLength = 25,
                     dom= 'Blfrtip', buttons = I('colvis'),
                     scrollX = TRUE,
                     scrollY = "500px" ,
                     autoWidth = T,
                     fixedColumns = list(leftColumns = 4),
                     columnDefs = list(list(visible = FALSE, targets = (ncol(value$data_industry)-1))
                )
                   ))  %>%
          formatStyle(0:ncol(value$data_seed), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
         formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
   )
}