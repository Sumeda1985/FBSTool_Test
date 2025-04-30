crop_production <- function(input,output,session){
  
observeEvent(input$startContinue,{
  # Get initial crop data for long format
value$cropDataLong <- value_database$data[
    CPCCode %in% unique(classification[classification %in% c("CP", "CD", "C"), CPCCode]) &
    ElementCode %in% c("5510", "5312","5025") &
    Year %in% c(2010:as.numeric(input$endyear)) &
    StatusFlag == 1,
    .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
  ][!is.na(Value)]
  
  # Get crop data for wide format processing
  cropData <- value_database$data[
    CPCCode %in% unique(classification[classification %in% c("CP", "CD", "C"), CPCCode]) &
    ElementCode %in% c("5510", "5312", "5025") &
    StatusFlag == 1 & Year %in% c(2010:input$endyear)
  ][!is.na(Value)]
  
  # Convert to wide format
  cropData <- wide_format(cropData)
  
  # Get column information
    yearcols <- grep("^[[:digit:]]{4}$", names(cropData), value = TRUE)
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  # Validate year range
  if (input$endyear > max(as.numeric(yearcols)) + 1) {
    yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
    value$data_crop <- NULL
    
    if (length(yearsToFill) > 0) {
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste(
          "Please compile Crop Production data for the year(s) ",
          paste(yearsToFill[1:(length(yearsToFill) - 1)], collapse = ", "),
          " first.",
          sep = ""
        ),
        type = "error"
      )
    }
  } else {
    # Process and visualize data
    cropData <- visualize_data(cropData, input, session)
    cropData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_crop <- cropData
    Add_table_version("crop", copy(value$data_crop))
  }
})
  
observeEvent(input$add_Crop, {
  showModal(viewCropTriplets())
})


#' Display crop triplets in a modal dialog
#' @param failed Boolean indicating if previous operation failed
#' @return A modalDialog object for displaying crop data
viewCropTriplets <- function(failed = FALSE) {
  modalDialog(
    easyClose = TRUE,
    size = "l",
    dataTableOutput("viewCrop"),
    footer = tagList(
      actionButton("cropInsert", "Insert")
    )
  )
}

output$viewCrop <- renderDataTable({
  # Get base crop list from classification
  croplistTool <- subset(classification, classification %in% c("C", "CD", "CP"))
  croplistTool[, classification := NULL]
  
  # Handle non-triplet cases
  non_triplet <- croplistTool[is.na(`Input Code`)]
  
  # Add special groundnut cases as requested by China
  groundnuts <- data.table(
    CPCCode = c("01422", "01421"),
    Commodity = c("Groundnuts in shell", "Groundnuts in shell, seed for planting"),
    `Output Code` = "5510",
    Output = "Production [t]"
  )
  non_triplet <- rbind(non_triplet, groundnuts, fill = TRUE)
  
  # Process triplets
  triplet <- croplistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
  
  # Filter and combine data
  classification_crop <- classification[classification %in% c("C", "CD", "CP")]
  cpc2keep <- unique(c(classification_crop$CPCCode, "01421", "01422"))
  non_triplet <- subset(non_triplet, CPCCode %in% cpc2keep)
  
  # Remove FBS codes
  fbscodes <- fread_rds("Data/fbsTree.rds")
  fbscodes <- c(unique(fbscodes$id1), unique(fbscodes$id2), 
                unique(fbscodes$id3), unique(fbscodes$id4))
  non_triplet <- subset(non_triplet, !(CPCCode %in% fbscodes))
  
  # Combine and clean final dataset
  croplistTool <- rbind(triplet, non_triplet)
  croplistTool[, c("Productivity Code", "Productivity") := NULL]
  
  # Prepare datatable with selection column
  DT <- croplistTool
  DT[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value="Row',
                          1:nrow(DT), '"><br>')
  DT <- subset(DT, !CPCCode %in% unique(value$data_crop$CPCCode))
  
  datatable(DT, escape = FALSE)
})


observeEvent(input$cropInsert, {
  selected_rows <- as.numeric(input$viewCrop_rows_selected)
  
  if (length(selected_rows) == 0) {
    data_current <- data.table(value$data_crop)
  } else {
    # Prepare crop list tool data
    croplistTool <- classification[classification %in% c("CP", "C", "CD")]
    croplistTool[, classification := NULL]
    non_triplet <- croplistTool[is.na(`Input Code`)]
    
    # Define codes to keep
    cpc2keep <- c(unique(classification$CPCCode), c("01421", "01422"))
    
    # Get FBS codes to exclude
    fbscodes <- fread_rds("Data/fbsTree.rds")
    fbscodes <- c(unique(fbscodes$id1), 
                  unique(fbscodes$id2), 
                  unique(fbscodes$id3), 
                  unique(fbscodes$id4))
    
    # Add special groundnut cases as requested
    groundnuts <- data.table(
      CPCCode = c("01422", "01421"),
      Commodity = c("Groundnuts in shell", "Groundnuts in shell, seed for planting"),
      `Output Code` = "5510",
      Output = "Production [t]"
    )
    
    # Process non-triplet data
    non_triplet <- rbind(non_triplet, groundnuts, fill = TRUE)[
      CPCCode %in% cpc2keep
    ][!(CPCCode %in% fbscodes)]
    
    # Process triplet data
    triplet <- croplistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
    
    # Combine and filter crop list
    croplistTool <- rbind(triplet, non_triplet)[
      !CPCCode %in% unique(isolate(value$data_crop$CPCCode))
    ]
    croplistTool[, c("Productivity Code", "Productivity") := NULL]
    
    # Process selected rows
    row_add <- croplistTool[selected_rows, ]
    row_add <- melt.data.table(
      row_add[, c("CPCCode", "Commodity", "Input Code", "Output Code")],
      id.vars = c("CPCCode", "Commodity")
    )[, variable := NULL]
    
    setnames(row_add, "value", "ElementCode")
    
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
    
    # Update data table
    data <- data.table(isolate(value$data_crop))
    data[, hidden := NULL]
    
    if (!(unique(row_add$CPCCode) %in% data$CPCCode)) {
      data <- rbind(row_add, data, fill = TRUE)[!is.na(ElementCode)]
      data[is.na(data)] <- ""
      yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
      data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
    }
    
    # Store inserted data and update main table
    value$insertdata <- row_add
    data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_crop <- data
  }
  
  # Save version history
  Add_table_version("crop", copy(value$data_crop))
})



#' Handle deletion of selected rows in crop table
observeEvent(input$delete_btn_crop, {
  # Create record of dropped data
  dropcropdata <- value$data_crop[
    as.numeric(input$crop_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5510", "5312"),
      StatusFlag = 0
    )
  ]
  
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropcropdata)
  value$data_crop <- value$data_crop[!(CPCCode %in% value$data_crop[
    as.numeric(input$crop_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("crop", copy(value$data_crop))
  
})
#' Download handler for crop production data
#' @description Exports crop production data to Excel format
output$downloadCrop <- createDownloadHandler(reactive(value$data_crop), 
                                             "crop_data.xlsx")

#here there is assumption that the country will insert commodities through the tool. 
#No filter for crop commodities. 
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileCropdenormalized", 
                         data_key = "data_crop", 
                         version_label =  "crop",
                         value_env = value)

observeEvent(input$uploadCropModal, {
  showModal(uploadCrop())
})

#' Create a modal dialog for uploading crop data
#' @description Creates a modal dialog with file upload and column selection inputs
#' @param failed Boolean indicating if previous operation failed
#' @return A modalDialog object containing the upload interface
uploadCrop <- function(failed = FALSE) {
  modalDialog(
    size = "l",
    titlePanel("Upload File"),
    
    # Main layout
    sidebarLayout(
      # Input panel
      sidebarPanel(
        # File upload input
        fileInput(
          "fileCrop",
          "Choose Excel File",
          multiple = TRUE,
          accept = NULL
        ),
        
        # Column selection inputs
        selectizeInput(
          "cpcCrop",
          "CPC Code",
          selected = NULL,
          choices = c("", colnames(value$data_cropCountry)),
          multiple = FALSE
        ),
        selectizeInput(
          "elementCrop",
          "Element Code",
          selected = NULL,
          choices = c("", colnames(value$data_cropCountry)),
          multiple = FALSE
        ),
        selectizeInput(
          "yearCrop",
          "Year",
          selected = NULL,
          choices = c("", colnames(value$data_cropCountry)),
          multiple = FALSE
        ),
        selectizeInput(
          "valueCrop",
          "Value",
          selected = NULL,
          choices = c("", colnames(value$data_cropCountry)),
          multiple = FALSE
        ),
        selectizeInput(
          "flagCrop",
          "Flag",
          selected = NULL,
          choices = c("", colnames(value$data_cropCountry)),
          multiple = FALSE
        ),
        
        # Upload button
        actionButton(
          "uploadCrop",
          "Upload Crop data"
        )
      ),
      
      # Output panel
      mainPanel(
        div(
          style = 'overflow-x: scroll',
          dataTableOutput('cropCountry')
        )
      )
    )
  )
}

output$cropCountry <- renderDataTable({
  req(input$fileCrop)
  inFile <- input$fileCrop
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  value$data_cropCountry <- DATA
  datatable(value$data_cropCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})
observe({
  # Update all column selection inputs with available choices
  updateSelectInput(session, "cpcCrop", 
                   choices = c("", colnames(value$data_cropCountry)))
  updateSelectInput(session, "elementCrop", 
                   choices = c("", colnames(value$data_cropCountry)))
  updateSelectInput(session, "yearCrop", 
                   choices = c("", colnames(value$data_cropCountry)))
  updateSelectInput(session, "valueCrop", 
                   choices = c("", colnames(value$data_cropCountry)))
  updateSelectInput(session, "flagCrop", 
                   choices = c("", colnames(value$data_cropCountry)))
})

observeEvent(input$uploadCrop, {
  normalized_upload(
    data_source = "data_cropCountry",
    input_cols = c("cpcCrop", "elementCrop", "yearCrop", "valueCrop", "flagCrop"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("5510"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_crop",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_crop",
    version_label = "crop",
    session = session,
    input=input
  )
})
#### undo crop ####
observeEvent(input$undoCrop, {
  # get last version
  new_version <- Pop_table_version("crop")
  # nothing to reset -- optionally a warning could be displayed in this case
  if(is.null(new_version)) {
    return()
  }
  value$data_crop <- new_version
})



observeEvent(input$crop_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('crop'),
    cell_edit_info = input$crop_cell_edit,
    data = value$data_crop,
    session = session,
    table_name = "crop"
  )
})
observeEvent(input$saveCrop, {
  # Prepare data for saving
  data_to_save <- copy(value$data_crop)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5510", "5312", "5025")]
  
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$cropDataLong,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_crop
  )
})
#' Render crop production data table
#' @description Displays the crop production data in an interactive table with formatting
output$crop <- renderDataTable({
  if (!is.null(value$data_crop)) {
    # Define table options
    table_options <- list(
      dom = 'Blfrtip',
      buttons = I('colvis'),
      pageLength = 25,
      scrollX = TRUE,
      scrollTo = TRUE,
      scrollY = "500px",
      autoWidth = TRUE,
      fixedColumns = list(leftColumns = 4),
      columnDefs = list(
        list(width = '150px', targets = c(3)),
        list(visible = FALSE, targets = (ncol(value$data_crop) - 1))
      )
    )
    
    # Create editable datatable
    datatable(
      value$data_crop,
      rownames = FALSE,
      class = 'cell-border stripe',
      editable = list(
        target = "cell",
        disable = list(columns = c(0:as.numeric(which(colnames(value$data_crop) == input$fromyear) - 2)))
      ),
      extensions = c("FixedColumns", "FixedHeader", "Buttons"),
      options = table_options
    ) %>%
      # Apply styling
      formatStyle(
        0:ncol(value$data_crop),
        valueColumns = "hidden",
        `border-bottom` = styleEqual(1, "solid 3px")
      ) %>%
      # Format numeric columns
      formatCurrency(
        columns = as.character(c(2010:input$endyear)),
        currency = "",
        digits = 0,
        interval = 3,
        mark = ","
      )
  }
})
}
