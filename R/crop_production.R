crop_production <- function(input,output,session){
  
observeEvent(input$startContinue,{
  # Get initial crop data for long format
  value$cropDataLong <- value_database$data[
    CPCCode %in% unique(classification[classification %in% c("CP", "CD", "C"), CPCCode]) &
    ElementCode %in% c("5510", "5312") &
    Year %in% c(2010:as.numeric(input$endyear)) &
    StatusFlag == 1,
    .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
  ][!is.na(Value)]
  
  # Get crop data for wide format processing
  cropData <- value_database$data[
    CPCCode %in% unique(classification[classification %in% c("CP", "CD", "C"), CPCCode]) &
    ElementCode %in% c("5510", "5312", "5025") &
    StatusFlag == 1
  ][!is.na(Value)]
  
  # Convert to wide format
  cropData <- wide_format(cropData)
  
  # Get column information
  flagcols <- grep("^Flag", names(cropData), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(cropData), value = TRUE)
  minyear <- min(as.numeric(yearcols))
  
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
    cropData <- visualize_data_production(cropData, input, session)
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
  fbscodes <- fread("Data/fbsTree.csv")
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
    fbscodes <- fread("Data/fbsTree.csv")
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
    value$insertcropdata <- row_add
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
  value$dropcropdata <- rbind(value$dropcropdata, dropcropdata)
  value$data_crop <- value$data_crop[!(CPCCode %in% value$data_crop[
    as.numeric(input$crop_rows_selected), unique(CPCCode)
  ])]
})


#' Download handler for crop production data
#' @description Exports crop production data to Excel format
output$downloadCrop <- downloadHandler(
  filename = function() {
    "crop_production.xlsx"
  },
  content = function(file) {
    # Prepare data for download
    data_download_crop <- data.table(value$data_crop)
    
    # Remove rows with missing CPCCode and hidden column
    data_download_crop <- data_download_crop[!is.na(CPCCode)]
    data_download_crop[, hidden := NULL]
    
    # Write to Excel file
    write.xlsx(data_download_crop, file, row.names = FALSE)
  }
)


# #upload crop denormalized data (#fileCrop)

observeEvent(input$fileCropdenormalized,{


  inFile <- input$fileCropdenormalized
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
  END_YEAR=input$endyear

  data_denormalized <- copy(DATA)
  data_denormalized <- long_format(data_denormalized)
  data_denormalized <- data_denormalized[CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])]
  data_denormalized <- data_denormalized[Year %in% c(input$fromyear : input$endyear)]
  data_denormalized[, c("Commodity","Element") := NULL]
  data_denormalized[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]

  crop_Data <- data.table(value$data_crop)
  crop_Data <- long_format(crop_Data)
  crop_Data[, c("Commodity","Element") := NULL]
  crop_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]

  xx=crop_Data[!is.na(Value)][
    data_denormalized,
    on = c("CPCCode", "ElementCode", "Year")
   ]
  xx[, c("Value","Flag"):= NULL]
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  crop_Data <- crop_Data[
    !xx,
    on = c("CPCCode", "ElementCode", "Year")]
  crop_Data <- rbind(crop_Data,xx)

  crop_Data <- merge(crop_Data, all_elements, by = "ElementCode", all.x = T)
  crop_Data <- merge(crop_Data, all_cpc, by= "CPCCode", all.x = T)
  crop_Data <- crop_Data[!is.na(Element)]
  crop_Data <- subset(crop_Data, Year %in% 2010:END_YEAR)
  crop_Data <- wide_format(crop_Data)
  crop_Data = visualize_data(crop_Data,END_YEAR)
  crop_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_crop <- crop_Data
  Add_table_version("crop", copy(value$data_crop))

})



observeEvent(input$uploadCropModal, {
  showModal(uploadCrop())
})



uploadCrop <- function(failed = FALSE) {


  modalDialog(size = "l",


              titlePanel("Upload File"),

              # Sidebar layout with input and output definitions ----
              sidebarLayout(

                # Sidebar panel for inputs ----
                sidebarPanel(

                  # Input: Select a file ----
                  fileInput("fileCrop", "Choose Excel File",
                            multiple = TRUE,
                            accept = NULL),
                  # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),

                  selectizeInput("cpcCrop", "CPC Code",
                                 selected = NULL, choices = c("",colnames( df_cropCountry$data_cropCountry)),multiple=F),
                  selectizeInput("elementCrop", "Element Code",
                                 selected = NULL, choices = c("",colnames( df_cropCountry$data_cropCountry)) ,multiple=F),
                  selectizeInput("yearCrop", "Year :",
                                 selected = NULL, choices = c("",colnames( df_cropCountry$data_cropCountry)),multiple=F),
                  selectizeInput("valueCrop", "Value :",
                                 selected = NULL, choices = c("",colnames( df_cropCountry$data_cropCountry)),multiple=F),
                  selectizeInput("flagCrop", "Flag :",
                                 selected = NULL, choices = c("",colnames( df_cropCountry$data_cropCountry)),multiple=F),
                  actionButton("uploadCrop","Upload Crop data")
                  ),

                # Main panel for displaying outputs ----
                mainPanel(
                  div(style = 'overflow-x: scroll', dataTableOutput('cropCountry'))
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
  df_cropCountry$data_cropCountry <- DATA
  datatable(df_cropCountry$data_cropCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})


observe({

  updateSelectInput(session, "cpcCrop", choices = c("", colnames( df_cropCountry$data_cropCountry)))
  updateSelectInput(session, "elementCrop", choices = c("",colnames( df_cropCountry$data_cropCountry)))
  updateSelectInput(session, "yearCrop", choices = c("",colnames( df_cropCountry$data_cropCountry)))
  updateSelectInput(session, "valueCrop", choices = c("",colnames( df_cropCountry$data_cropCountry)))
  updateSelectInput(session, "flagCrop", choices = c("",colnames( df_cropCountry$data_cropCountry)))

})

#############################################
observeEvent(input$uploadCrop, {
  removeModal()
})

observeEvent(input$uploadCrop,{
  data <- data.table(df_cropCountry$data_cropCountry)
  if(input$cpcCrop == ""|input$elementCrop == ""| input$yearCrop == ""| input$valueCrop == ""| input$flagCrop == "" ){
    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = "Invalid data",
      type = "warning"
    )
    value$data_crop <- value$data_crop

  } else{
  data <- data[, c(input$cpcCrop, input$elementCrop, input$yearCrop, input$valueCrop, input$flagCrop), with= F]
    if (length(names(data)[duplicated(names(data))])>0){
    sendSweetAlert(
      session = session,
      title = "WARNING !!",
      text = "Please select the colums correctly",
      type = "warning"
    )
    data <- data.table(df_cropCountry$data_cropCountry)
  }
    else{
    setnames(data,c(input$cpcCrop, input$elementCrop, input$yearCrop, input$valueCrop, input$flagCrop),
           c("CPCCode","ElementCode","Year","Value","Flag"))
    #extract only crop commodities
   data <- data[CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])]
   data <- subset(data, ElementCode %in% c("5312","5510"))

  data[, Year := as.character(Year)]
  data[, CPCCode := as.character(CPCCode)]
  data[, ElementCode := as.character(ElementCode)]
  data <- data[Year %in% c(input$fromyear : input$endyear)]

  crop <- data.table(value$data_crop)
  crop <- long_format(crop)
  crop[,c("Commodity","Element") := NULL]
  crop[, ElementCode := as.character(ElementCode)]
  xx <- crop[!is.na(Value)][
  data, on = c("CPCCode", "ElementCode", "Year")]
  xx[, c("Value","Flag"):= NULL]
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  crop <- crop[!is.na(Value)][
    !xx,
    on = c("CPCCode", "ElementCode", "Year")
   ]
  crop <- rbind(crop,xx)
  crop <- merge(crop, all_elements, by = "ElementCode", all.x = T)
  crop <- merge(crop, all_cpc, by= "CPCCode", all.x = T)
  crop <- crop[!is.na(Element)]
  crop <- wide_format(crop)
  crop[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_crop <- crop
  Add_table_version("crop", copy(value$data_crop))
  }
    }
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


##############################################


#save crop

observeEvent(input$saveCrop,{
  data_to_save <- copy(value$data_crop)
  data_to_save[,hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5510", "5312","5025")]
#invalidateLater(2000)
 # if(rv$active_sessions[1] != session$token){
   # shinyalert(text = "Data is saved by another User. Please save your current work and Refresh", type = "warning")
 #}
save_to_database(data = data_to_save,year_range = c(input$fromyear:input$endyear),session,input,output)
 })

output$crop <-

  renderDataTable({
    if(!is.null(value$data_crop)){
    datatable (value$data_crop, rownames= FALSE,class = 'cell-border stripe',
      editable = list(
      target = "cell",
      disable = list(columns = c(0:as.numeric(which( colnames(value$data_crop) == input$fromyear)-2)))),
      extensions = c("FixedColumns","FixedHeader", "Buttons"),
       options = list(
                dom= 'Blfrtip',
                buttons = I('colvis'),
                pageLength = 25,
                 # paging = TRUE,
                 # sort = TRUE,
                 scrollX = TRUE,
                 scrollTo = TRUE,
                 scrollY = "500px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(list(width = '150px', targets = c(3)),
                                   list(visible = FALSE, targets = (ncol(value$data_crop)-1))
                 )
               ))  %>%
      formatStyle(0:ncol(value$data_crop), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    }
  })
}
