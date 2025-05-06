
livestock_production <- function(input, output, session) {

observeEvent(input$startContinue ,{
   # Get initial livestock data for long format
     value$livestockDataLong <- value_database$data[
      CPCCode %in% unique(classification[classification %in% c("LP", "LD", "L"), CPCCode]) &
        ElementCode %in% c("5510", "5318", "5319", "5320","5314", "5327", "5313", "5321") &
        Year %in% c(2010:as.numeric(input$endyear)) &
        StatusFlag == 1,
      .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
    ][!is.na(Value)]
    
    # Get livestock data for wide format processing
    livestockData <- value_database$data[
      CPCCode %in% unique(classification[classification %in% c("LP", "LD", "L"), CPCCode]) &
        ElementCode %in% c("5510", "5318", "5319", "5320","5314", "5327", "5313", "5321") &
        StatusFlag == 1 & Year %in% c(2010:input$endyear)
    ][!is.na(Value)]
    
    # Remove duplicates
    livestockData <- livestockData[
      !duplicated(
        livestockData, 
        by = c("CPCCode", "Commodity", "ElementCode", "Element", "Year")
      )
    ]
    
    # Handle slaughter data
    remove_slaughter <- livestockData[
      ,  `:=`( COUNT = .N  ), 
      by = c("CPCCode", "Year")
    ][
      , no_prod := ifelse(COUNT == 1 & ElementCode != "5510", 1, 0)
    ]
    slaughter_codes_to_remove <- unique(
      remove_slaughter[COUNT == 1 & no_prod == 1]$CPCCode
    )
    
    livestockData <- livestockData[
      !is.na(Value) &
        !CPCCode %in% slaughter_codes_to_remove &
        ElementCode != "5327"
    ]
    
    livestockData <- wide_format(livestockData)
    
    # Get column information
    yearcols <- grep("^[[:digit:]]{4}$", names(livestockData), value = TRUE)
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
    
    if (input$endyear > max(as.numeric(yearcols)) + 1) {
      yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
      value$data_livestock <- NULL
      
      if (length(years_to_fill) > 0) {
        years_msg <- paste(
          years_to_fill[1:(length(years_to_fill) - 1)], 
          collapse = ", "
        )
        sendSweetAlert(
          session = session,
          title = "Error!",
          text = sprintf(
            "Please compile Livestock Production data for the year(s) %s first.",
            years_msg
          ),
          type = "error"
        )
      } 
    }else {
      # Process and visualize data
      livestockData <- visualize_data(livestockData, input, session)
      livestockData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_livestock <- livestockData
      Add_table_version("livestock", copy(value$data_livestock))
    } 
})

observeEvent(input$add_Livestock, {
    showModal(viewLivestockTriplets())
  })
  
  viewLivestockTriplets <- function(failed = FALSE) {
    modalDialog(easyClose = TRUE, size= "l",dataTableOutput("viewLivestock"),
    footer = tagList(actionButton("LivestockInsert", "Insert")))
}  
output$viewLivestock= renderDataTable({
    # Get livestock classification data
    livestocklistTool <- classification[ classification %in% c("L", "LD", "LP")]
    livestocklistTool[, classification := NULL]
    
    # Separate non-triplet and triplet items
    non_triplet <- livestocklistTool[is.na(`Input Code`)]
    triplet <- livestocklistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
    
    #filter CPC codes
    cpc2keep <- unique(livestocklistTool$CPCCode)
    
    # Filter non-triplet items
    non_triplet <- subset(non_triplet, CPCCode %in% cpc2keep)
    
    # Filter out FBS codes
    fbscodes <- data.table(dbReadTable(concore, name="fbs_tree"))
    fbscodes <- c(
      unique(fbscodes$id1),
      unique(fbscodes$id2),
      unique(fbscodes$id3),
      unique(fbscodes$id4)
    )
    non_triplet <- subset(non_triplet, !(CPCCode %in% fbscodes))
    
    # Combine triplet and non-triplet data
    livestocklistTool <- rbind(triplet, non_triplet)
    
    # Remove unnecessary columns
    livestocklistTool[, c("Productivity Code", "Productivity") := NULL]
    
    # Create data table with selection checkboxes
    DT <- livestocklistTool
    DT[["Select"]] <- paste0(
      '<input type="checkbox" name="row_selected" value="Row',
      1:nrow(DT),
      '"><br>'
    )
    
    # Filter out existing CPC codes
    DT <- subset(DT, !CPCCode %in% unique(value$data_livestock$CPCCode))
    
    # Return formatted datatable
    datatable(DT, escape = FALSE) 
})
  
observeEvent(input$LivestockInsert, {
    removeModal()
  
  })
  
# Table editing handlers
  observeEvent(input$livestock_cell_edit, {
    handle_cell_edit(
    proxy = dataTableProxy('livestock'),
    cell_edit_info = input$livestock_cell_edit,
    data = value$data_livestock,
    session = session,
    table_name = "livestock"
  )
})
  
observeEvent(input$LivestockInsert, {
    selected_rows <- as.numeric(input$viewLivestock_rows_selected)
    if (length(selected_rows) == 0){
     data.table(value$data_livestock)
    }
 else {
     livestocklistTool <-classification[ classification %in% c("LP","L","LD")][,classification := NULL]
      cpc2keep= unique(livestocklistTool$CPCCode)
      non_triplet= livestocklistTool[is.na(`Input Code`)][CPCCode %in% cpc2keep]
      triplet= livestocklistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
      
      # Get FBS codes to exclude
      fbscodes <- data.table(dbReadTable(concore, name="fbs_tree"))
      fbscodes <- c(unique(fbscodes$id1), 
                    unique(fbscodes$id2), 
                    unique(fbscodes$id3), 
                    unique(fbscodes$id4))
      
      non_triplet <- subset(non_triplet, !(CPCCode %in% fbscodes))
      livestocklistTool <- rbind(triplet,non_triplet)[ !CPCCode %in% 
                                                         unique(isolate(value$data_livestock$CPCCode))]
      livestocklistTool[,c("Productivity Code", "Productivity") := NULL]
      row_add  <- livestocklistTool[selected_rows,]
      row_add <- melt.data.table(
        row_add[,c("CPCCode", "Commodity", "Input Code", "Output Code")], 
        id.vars = c("CPCCode", "Commodity"))[, variable:=NULL]
      setnames(row_add,"value", "ElementCode")
      # Merge with element information
      row_add <- merge(
        row_add, 
        all_elements, 
        by.x = "ElementCode",
        by.y = "ElementCode",
        all.x = TRUE
      )
      # Set column order and sort
      setcolorder(row_add, c("CPCCode", "Commodity", "ElementCode", "Element"))
      row_add <- row_add[order(CPCCode)]
      data= data.table(isolate(value$data_livestock))[, hidden := NULL]
      
      if (!(unique(row_add$CPCCode) %in% data$CPCCode)){
        data=rbind(row_add,data,fill=T)[!is.na(ElementCode)]
        data[is.na(data)] <- ""
        yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
        data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
 }
      value$insertdata <- row_add
      data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_livestock <- data
 }
    Add_table_version("livestock", copy(value$data_livestock))
})
  
# Undo functionality
  observeEvent(input$undoLivestock, {
    new_version <- Pop_table_version("livestock")
    if (!is.null(new_version)) {
      value$data_livestock <- new_version
    }
  })
  
  # Delete functionality
  observeEvent(input$delete_btn_livestock, {
   droplivestockdata <- value$data_livestock[
        as.numeric(input$livestock_rows_selected),
        .(
          CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
          Country = input$countrym49,
          CPCCode,
          ElementCode = c("5510","5318" ,
                          "5319", "5320" ,"5314" ,"5327" ,"5313" ,"5321"),StatusFlag = 0
        )
      ]
    value$dropdata <- rbind(value$dropdata, droplivestockdata)
    value$data_livestock <- value$data_livestock[!(CPCCode %in% value$data_livestock[
      as.numeric(input$livestock_rows_selected), unique(CPCCode)
    ])]
      
  Add_table_version("livestock", copy(value$data_livestock))
 
  })
 # Download handler
  output$downloadLivestock <- createDownloadHandler(reactive(value$data_livestock), 
                                                    "livestock_data.xlsx"
  )
    
    
    
uploadLivestock <- function(failed = FALSE) {
    modalDialog(
      size = "l",
      titlePanel("Upload File"),
      sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
          # File input
          fileInput(
            "fileLivestock", 
            "Choose Excel File",
            multiple = TRUE,
            accept = NULL
          ),
          
          # Column selection inputs
          selectizeInput(
            "cpcLivestock", 
            "CPC Code",
            selected = NULL, 
            choices = c("", colnames(value$data_livestockCountry)),
            multiple = FALSE
          ),
          
          selectizeInput(
            "elementLivestock", 
            "Element Code",
            selected = NULL, 
            choices = c("", colnames(value$data_livestockCountry)),
            multiple = FALSE
          ),
          
          selectizeInput(
            "yearLivestock", 
            "Year :",
            selected = NULL, 
            choices = c("", colnames(value$data_livestockCountry)),
            multiple = FALSE
          ),
          
          selectizeInput(
            "valueLivestock", 
            "Value :",
            selected = NULL, 
            choices = c("", colnames(value$data_livestockCountry)),
            multiple = FALSE
          ),
          
          selectizeInput(
            "flagLivestock", 
            "Flag :",
            selected = NULL, 
            choices = c("", colnames(value$data_livestockCountry)),
            multiple = FALSE
          ),
          
          actionButton("uploadLivestock", "Upload Livestock data")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
          div(
            style = 'overflow-x: scroll', 
            dataTableOutput('livestockCountry')
          )
        )
      )
    )
  }  
  
  observeEvent(input$uploadLivestock, {
    removeModal()
 }) 
# Upload handlers
  observeEvent(input$uploadLivestockModal, {
    showModal(uploadLivestock())
  })
observeEvent(input$uploadLivestock, {
    normalized_upload(
      data_source = "data_livestockCountry",
      input_cols = c("cpcLivestock", "elementLivestock", "yearLivestock", "valueLivestock", "flagLivestock"),
      std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
      filter_element_code = c("5510"),
      from_year = input$fromyear,
      to_year = input$endyear,
      existing_data = "data_livestock",
      all_elements = all_elements,
      all_cpc = all_cpc,
      value_env = value,
      value_name = "data_livestock",
      version_label = "livestock",
      session = session,
      input = input
    )
  })
  
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileLivestockdenormalized", 
                         data_key = "data_livestock", 
                         version_label =  "livestock",
                         value_env = value)
  
  
  
  output$livestockCountry <- renderDataTable({
    req(input$fileLivestock)
    inFile <- input$fileLivestock
    file_path <- paste(inFile$datapath, ".xlsx", sep = "")
    file.rename(inFile$datapath, file_path)
    
    value$data_livestockCountry <- read_excel(file_path, 1)
    datatable(
      value$data_livestockCountry,
      options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
    )
  })
  
observe({
updateSelectInput(session, "cpcLivestock", choices = c("", colnames( value$data_livestockCountry)))
updateSelectInput(session, "elementLivestock", choices = c("",colnames( value$data_livestockCountry)))
updateSelectInput(session, "yearLivestock", choices = c("",colnames( value$data_livestockCountry)))
updateSelectInput(session, "valueLivestock", choices = c("",colnames( value$data_livestockCountry)))
updateSelectInput(session, "flagLivestock", choices = c("",colnames( value$data_livestockCountry)))
}) 
  
observeEvent(input$saveLivestock,{
  data_to_save <- copy(value$data_livestock)
  data_to_save[,hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5510", "5318", "5319", "5320","5314", "5327", "5313", "5321")]
  
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$livestockDataLong,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_livestock
  )

})  
  
output$livestock <- renderDataTable({
  if (!is.null(value$data_livestock)) {
    datatable(
      value$data_livestock,
      rownames = FALSE,
      class = 'cell-border stripe',
      editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which(colnames(value$data_livestock) == input$fromyear)-2)))),
      extensions = c("FixedColumns", "FixedHeader", "Buttons"),
      options = list(
        pageLength = 25, dom = 'Blfrtip', buttons = I('colvis'),
        scrollX = TRUE, scrollY = "500px", autoWidth = TRUE,
        fixedColumns = list(leftColumns = 4),
        columnDefs = list(
          list(width = '150px', targets = c(3)),
          list(visible = FALSE, targets = (ncol(value$data_livestock)-1))
        )
      )
    ) %>%
      formatStyle(0:ncol(value$data_livestock), valueColumns = "hidden", `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatCurrency(as.character(c(2010:input$endyear)), currency = "", digits = 0, interval = 3, mark = ",")
  }
})  
  
} 