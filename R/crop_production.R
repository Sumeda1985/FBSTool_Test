

crop_production <- function(input,output,session){


 observeEvent(input$startContinue,{
    END_YEAR=input$endyear

    cropDataLong=subset(value_database$data,
                        CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])
                  & ElementCode %in% c("5510","5312") ) #area sown is removed from production and added to seed
    crop_production_data <- subset(value_database$data, CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode]))
    setDT(cropDataLong)
    cropDataLong <- cropDataLong[!duplicated(cropDataLong,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
    cropDataLong[, c("CountryM49","Country"):=NULL]
    cropDataLong <- subset(cropDataLong, Year %in% c(2010 : END_YEAR) )
    cropDataLong <- cropDataLong[!is.na(Value)]
    cropDataLong <- cropDataLong[!ElementCode == "5327"]
    value$cropDataLong <- cropDataLong
    cropData=wide_format(cropDataLong)
    flagcols <- grep("^Flag", names(cropData), value = TRUE)
    yearcols <- grep("^[[:digit:]]{4}$", names(cropData), value = TRUE)
    minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))

  if(END_YEAR > maxyear +1){
    END_YEAR=as.numeric(END_YEAR)
    yearsToFill = (maxyear + 1):END_YEAR

    value$data_crop <- NULL
    if(length(yearsToFill) > 0){
      # stop(paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Crop Prodcution data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
}

  }  else {

    cropData = visualize_data_production(cropData,END_YEAR, session)
    cropData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_crop <- cropData
    Add_table_version("crop", copy(value$data_crop))
  }

})



observeEvent(input$add_Crop, {
  showModal(viewCropTriplets())
})


viewCropTriplets <- function(failed = FALSE) {
  modalDialog(

    easyClose = TRUE, size= "l",
    dataTableOutput("viewCrop")
    ,

    footer = tagList(

      actionButton("cropInsert", "Insert")
    )
  )

}



output$viewCrop= renderDataTable({

  croplistTool <-subset(classification, classification %in% c("C", "CD", "CP"))
  croplistTool[,classification := NULL]
  non_triplet= croplistTool[is.na(`Input Code`)]

  #Requested by China to add 01421 anf 01422 to the crop list.
  groundnuts <- data.table(CPCCode = c("01422","01421"), Commodity = c("Groundnuts in shell","Groundnuts in shell, seed for planting"), `Output Code` = "5510",Output = "Production [t]")
  non_triplet <- rbind(non_triplet,groundnuts, fill=T)
  triplet= croplistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]

  classification_crop  <- classification[classification %in% c("C", "CD", "CP")]
  cpc2keep= unique(classification_crop$CPCCode)
  ##Requested by China to add 01421 anf 01422 to the crop list.
  cpc2keep <- c(cpc2keep, c("01421","01422"))
  non_triplet=subset(non_triplet, CPCCode %in% cpc2keep)
  fbscodes=fread("Data/fbsTree.csv")
  fbscodes=c(unique(fbscodes$id1),unique(fbscodes$id2),unique(fbscodes$id3),unique(fbscodes$id4))
  non_triplet=subset(non_triplet, !(CPCCode %in% fbscodes))
  croplistTool=rbind(triplet,non_triplet)
  croplistTool[,c("Productivity Code", "Productivity") := NULL]

  DT=croplistTool
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  DT <- subset(DT,  !CPCCode %in% unique(value$data_crop$CPCCode) )

  datatable(DT,
            escape=F)


})


observeEvent(input$cropInsert, {

  removeModal()

})


proxy_crop = dataTableProxy('crop')

observeEvent(input$crop_cell_edit, {
info = input$crop_cell_edit

  i = info$row
  j = (info$col + 1)
  v = info$value
  value$data_crop[i,(j) := v]
  replaceData(proxy_crop, value$data_crop, resetPaging = FALSE,rownames = FALSE)  # important
  info1 <- input[["crop_cell_edit"]]
  i <- info1[["row"]]
  j <- info1[["col"]]
  runjs(colorizeCell(i, j+1,"crop"))

  Add_table_version("crop", copy(value$data_crop))

})





observeEvent(input$cropInsert, {

   s=as.numeric(input$viewCrop_rows_selected)

if (length(s) == 0){

    data_current <- data.table(value$data_crop)
    value$data_crop <- data_current
}
 else {
    croplistTool <-subset(classification, classification %in% c("CP","C","CD"))
    croplistTool[,classification := NULL]
    non_triplet= croplistTool[is.na(`Input Code`)]

    #Requested by China to add 01421 anf 01422 to the crop list.

    groundnuts <- data.table(CPCCode = c("01422","01421"), Commodity = c("Groundnuts in shell","Groundnuts in shell, seed for planting"), `Output Code` = "5510",Output = "Production [t]")
    non_triplet <- rbind(non_triplet,groundnuts, fill=T)
    triplet= croplistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
    classification_crop=classification[classification %in% c("C", "CD", "CP")]
    cpc2keep= unique(classification$CPCCode)
    cpc2keep <- c(cpc2keep,c("01421","01422"))
    non_triplet=subset(non_triplet, CPCCode %in% cpc2keep)
    fbscodes=fread("Data/fbsTree.csv")
    fbscodes=c(unique(fbscodes$id1),unique(fbscodes$id2),unique(fbscodes$id3),unique(fbscodes$id4))
    non_triplet=subset(non_triplet, !(CPCCode %in% fbscodes))
    croplistTool=rbind(triplet,non_triplet)
    croplistTool <- subset(croplistTool,  !CPCCode %in% unique(isolate(value$data_crop$CPCCode) ))
    croplistTool[,c("Productivity Code", "Productivity") := NULL]


    yy=croplistTool[s,]
    # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff[,variable:=NULL]
    setnames(ff,"value", "ElementCode")

    elementName = read_excel("Data/Reference File.xlsx",sheet = "Elements")
    elementName = data.table(elementName)

    oo=merge(ff,elementName, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    oo=oo[order(CPCCode)]

    data=isolate(value$data_crop)
    data=data.table(data)
    data[, hidden := NULL]

    if (!(unique(oo$CPCCode) %in% data$CPCCode)){

      data=rbind(oo,data,fill=T)
      data=data[!is.na(ElementCode)]
      data[is.na(data)] <- ""
      yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
      data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
 }

    data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_crop <- data
}
    Add_table_version("crop", copy(value$data_crop))

})


#delete rows in crop table

observeEvent(input$delete_btn_crop, {
  dropdata <- value$data_crop[as.numeric(input$crop_rows_selected),
                              .(CountryM49=countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
                                Country=input$countrym49,
                                CPCCode,
                                ElementCode=c("5510", "5312"),
                                StatusFlag=0)]
  rows_update(contbl, as_tibble(dropdata), 
              by = c("CountryM49", "Country",
                     "CPCCode",
                     "ElementCode"),
              in_place=TRUE, copy = TRUE, unmatched="ignore")
  value$data_crop <- value$data_crop[!(CPCCode %in% value$data_crop[as.numeric(input$crop_rows_selected),unique(CPCCode)])]
})


#download excle file (#downloadCrop)

output$downloadCrop <- downloadHandler(

  filename = function() {

    "crop_production.xlsx"
  },

  content = function(file) {

    data_download_crop <- data.table(value$data_crop)
    data_download_crop <- data_download_crop[!is.na(CPCCode)]
    data_download_crop[,hidden := NULL]
    write.xlsx(data_download_crop ,file,row.names = FALSE)
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
  data_to_save <- subset(data_to_save, ElementCode %in% c("5510", "5312","5025"))


  invalidateLater(2000)
  if(rv$active_sessions[1] != session$token){
    shinyalert(text = "Data is saved by another User. Please save your current work and Refresh", type = "warning")
 }

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
