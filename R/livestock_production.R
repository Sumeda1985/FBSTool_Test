livestock_production <- function(input,output,session){

observeEvent(input$startContinue,{
END_YEAR=input$endyear
# cropData=subset(countryData, CPCCode %in% unique(subset(classification, classification %in% c("CP","CD","C"))[,CPCCode])
 #                 & ElementCode %in% c("5510","5312","5421","5327","5423") )
livestockData=subset(value_database$data, CPCCode %in% unique(subset(classification, classification %in% c("LP","LD","L"))[,CPCCode])
                  & ElementCode %in% c("5510","5318" ,"5319", "5320" ,"5314" ,"5327" ,"5313" ,"5321") )
#take production that do not appear in classification table
livestock_production_data <- subset(value_database$data, CPCCode %in% unique(subset(classification, classification %in% c("LP","LD","L"))[,CPCCode]))
setDT(livestockData)
livestockData <- livestockData[!duplicated(livestockData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
#remove slaughter where there is no production
remove_slaughter <- copy(livestockData)
remove_slaughter <- remove_slaughter[ , `:=`( COUNT = .N  ) , by = c("CPCCode","Year") ]
remove_slaughter <- remove_slaughter[, no_prod := ifelse(COUNT == 1 & ElementCode != "5510",1,0)]
remove_slaughter <- unique(remove_slaughter[COUNT == 1 & no_prod == 1]$CPCCode)
livestockData[, c("CountryM49","Country"):=NULL]
livestockData <- subset(livestockData, Year %in% c(2010 : END_YEAR) )
livestockData <- livestockData[!is.na(Value)]
livestockData <- livestockData[!CPCCode %in% remove_slaughter]
livestockData <- livestockData[!ElementCode == "5327"]
livestockData=wide_format(livestockData)
  
flagcols <- grep("^Flag", names(livestockData), value = TRUE)
yearcols <- grep("^[[:digit:]]{4}$", names(livestockData), value = TRUE)
minyear <- min(as.numeric(yearcols))
maxyear <- max(as.numeric(yearcols))
  if (END_YEAR > maxyear +1){
    END_YEAR=as.numeric(END_YEAR)
    yearsToFill = (maxyear + 1):END_YEAR
    value$data_livestock <- NULL
    if(length(yearsToFill) > 0){
      # stop(paste("Please compile FBS for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Livestock Production data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
    }
    
  } else{
    livestockData = visualize_data_production(livestockData,END_YEAR, session)
    livestockData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_livestock <- livestockData 
    Add_table_version("livestock", copy(value$data_livestock))
  }
})

observeEvent(input$add_Livestock, {
  showModal(viewLivestockTriplets())
})

viewLivestockTriplets <- function(failed = FALSE) {
  modalDialog(
    easyClose = TRUE, size= "l",
    dataTableOutput("viewLivestock"),
    footer = tagList(actionButton("LivestockInsert", "Insert") )
  )
}

output$viewLivestock= renderDataTable({
  # croplistTool=fread("Data/cropListTool.csv")
  livestocklistTool <-subset(classification, classification %in% c("L", "LD", "LP"))
  livestocklistTool[,classification := NULL]
  non_triplet= livestocklistTool[is.na(`Input Code`)] 
  triplet= livestocklistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
  
  classification_livestock  <- classification[classification %in% c("L", "LD", "LP")]
  cpc2keep= unique(classification_livestock$CPCCode)
  non_triplet=subset(non_triplet, CPCCode %in% cpc2keep)
  fbscodes=fread("Data/fbsTree.csv")
  fbscodes=c(unique(fbscodes$id1),unique(fbscodes$id2),unique(fbscodes$id3),unique(fbscodes$id4))
  non_triplet=subset(non_triplet, !(CPCCode %in% fbscodes))
  livestocklistTool=rbind(triplet,non_triplet)
  livestocklistTool[,c("Productivity Code", "Productivity") := NULL]
  
  DT=livestocklistTool
  DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
  DT <- subset(DT,  !CPCCode %in% unique(value$data_livestock$CPCCode) )
  datatable(DT,escape=F)
})

observeEvent(input$LivestockInsert, {
  removeModal()
})

proxy_livestock = dataTableProxy('livestock')
observeEvent(input$livestock_cell_edit, {
  info = input$livestock_cell_edit
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  value$data_livestock[i,(j) := v]
  replaceData(proxy_livestock, value$data_livestock, resetPaging = FALSE,rownames = FALSE)  # important

  info1 <- input[["livestock_cell_edit"]]
  i <- info1[["row"]]
  j <- info1[["col"]]
  runjs(colorizeCell(i, j+1,"livestock"))
  Add_table_version("livestock", copy(value$data_livestock))
})

observeEvent(input$undoLivestock, {
  # get last version
  new_version <- Pop_table_version("livestock")  
  # nothing to reset -- optionally a warning could be displayed in this case
  if(is.null(new_version)) {
    return()
}
  value$data_livestock <- new_version
})

observeEvent(input$LivestockInsert, {
  s=as.numeric(input$viewLivestock_rows_selected)
  
  if (length(s) == 0){
    data_current <- data.table(value$data_livestock)
    value$data_livestock <- data_current
   }
  
  else {
    livestocklistTool <-subset(classification, classification %in% c("LP","L","LD"))
    livestocklistTool[,classification := NULL]
    non_triplet= livestocklistTool[is.na(`Input Code`)]
    triplet= livestocklistTool[!(CPCCode %in% unique(non_triplet$CPCCode))]
    classification_livestock=classification[classification %in% c("L", "LD", "LP")]
    cpc2keep= unique(classification_livestock$CPCCode)
    non_triplet=subset(non_triplet, CPCCode %in% cpc2keep)
    fbscodes=fread("Data/fbsTree.csv")
    fbscodes=c(unique(fbscodes$id1),unique(fbscodes$id2),unique(fbscodes$id3),unique(fbscodes$id4))
    non_triplet=subset(non_triplet, !(CPCCode %in% fbscodes))
    livestocklistTool=rbind(triplet,non_triplet)
    livestocklistTool <- subset(livestocklistTool,  !CPCCode %in% unique(isolate(value$data_livestock$CPCCode) ))
    livestocklistTool[,c("Productivity Code", "Productivity") := NULL]
    yy=livestocklistTool[s,]
    # ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Productivity Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff=melt.data.table(yy[,c("CPCCode", "Commodity", "Input Code", "Output Code")], id.vars = c("CPCCode", "Commodity"))
    ff[,variable:=NULL]
    setnames(ff,"value", "ElementCode")
    elementName = data.table(read_excel("Data/Reference File.xlsx",sheet = "Elements"))
    oo=merge(ff,elementName, by.x = "ElementCode",by.y = "ElementCode",all.x  = T)
    setcolorder(oo,c("CPCCode", "Commodity", "ElementCode", "Element"))
    oo=oo[order(CPCCode)]
    data=isolate(value$data_livestock)
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
    value$data_livestock <- data
   }
  Add_table_version("livestock", copy(value$data_livestock))
})
#delete rows in crop table
observeEvent(input$delete_btn_livestock, {
  t = copy(value$data_livestock)
  final_data <- copy(value$data_livestock)
  if (!is.null(input$livestock_rows_selected)) {
    t <- t[as.numeric(input$livestock_rows_selected),]
    cpc_code_to_remove <- unique(t$CPCCode)
    ele_code_to_remove <- unique(t$ElementCode)
  }
  value$data_livestock<- final_data[!CPCCode %in% cpc_code_to_remove]
  #remove the cpc with the element from the database
  database <- copy(value_database$data)
  database <- database[!(CPCCode %in% cpc_code_to_remove & ElementCode %in% c("5510","5318" ,
                                                                              "5319", "5320" ,"5314" ,"5327" ,"5313" ,"5321"))]
  # save(database, file = "Data/countrySUA.RData")  
  value_database$data <<- database
  Add_table_version("livestock", copy(value$data_livestock))
})

#download excle file (#downloadCrop)
output$downloadLivestock<- downloadHandler(
  filename = function() {
    "livestock_production.xlsx"
  },
  content = function(file) {
    data_download_livestock <- data.table(value$data_livestock)
    data_download_livestock <- data_download_livestock[!is.na(CPCCode)]
    data_download_livestock[,hidden := NULL]
    write.xlsx(data_download_livestock ,file,row.names = FALSE)
  }
)

observeEvent(input$uploadLivestockModal, {
  showModal(uploadLivestock())
})

uploadLivestock <- function(failed = FALSE) {
  modalDialog(size = "l",
             titlePanel("Upload File"),
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
              # Sidebar panel for inputs ----
              sidebarPanel(
              # Input: Select a file ----
                  fileInput("fileLivestock", "Choose Excel File",
                            multiple = TRUE,
                            accept = NULL),
                  # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                  selectizeInput("cpcLivestock", "CPC Code",
                                 selected = NULL, choices = c("",colnames( df_livestockCountry$data_livestockCountry)),multiple=F),
                  selectizeInput("elementLivestock", "Element Code",
                                 selected = NULL, choices = c("",colnames( df_livestockCountry$data_livestockCountry)) ,multiple=F),
                  selectizeInput("yearLivestock", "Year :",
                                 selected = NULL, choices = c("",colnames( df_livestockCountry$data_livestockCountry)),multiple=F),
                  selectizeInput("valueLivestock", "Value :",
                                 selected = NULL, choices = c("",colnames( df_livestockCountry$data_livestockCountry)),multiple=F),
                  selectizeInput("flagLivestock", "Flag :",
                                 selected = NULL, choices = c("",colnames( df_livestockCountry$data_livestockCountry)),multiple=F),
                  actionButton("uploadLivestock","Upload Livestock data")
              ),
                # Main panel for displaying outputs ----
                mainPanel(
                  div(style = 'overflow-x: scroll', dataTableOutput('livestockCountry'))
                )
             )
  )
}

output$livestockCountry <- renderDataTable({
  req(input$fileLivestock)
  inFile <- input$fileLivestock
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  df_livestockCountry$data_livestockCountry <- DATA
  datatable(df_livestockCountry$data_livestockCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})

observe({
  updateSelectInput(session, "cpcLivestock", choices = c("", colnames( df_livestockCountry$data_livestockCountry)))
  updateSelectInput(session, "elementLivestock", choices = c("",colnames( df_livestockCountry$data_livestockCountry)))
  updateSelectInput(session, "yearLivestock", choices = c("",colnames( df_livestockCountry$data_livestockCountry)))
  updateSelectInput(session, "valueLivestock", choices = c("",colnames( df_livestockCountry$data_livestockCountry)))
  updateSelectInput(session, "flagLivestock", choices = c("",colnames( df_livestockCountry$data_livestockCountry)))
})

observeEvent(input$uploadLivestock, {
  removeModal()
})

observeEvent(input$uploadLivestock,{
  data <- data.table(df_livestockCountry$data_livestockCountry)
  if(input$cpcLivestock == ""|input$elementLivestock == ""| input$yearLivestock == ""| 
     input$valueLivestock == ""| input$flagLivestock == "" ){
    sendSweetAlert(
      session = session,
      title = "Warning !!",
      text = "Invalid data",
      type = "warning"
    )
   value$data_livestock <- value$data_livestock
 }else{
  data <- data[, c(input$cpcLivestock, input$elementLivestock, input$yearLivestock, input$valueLivestock, input$flagLivestock), with= F]
  if (length(names(data)[duplicated(names(data))])>0){
    sendSweetAlert(
      session = session,
      title = "WARNING !!",
      text = "Please select the colums correctly",
      type = "warning"
    )
    data <- data.table(df_livestockCountry$data_livestockCountry)
  }
  else{
  setnames(data,c(input$cpcLivestock, input$elementLivestock, input$yearLivestock, input$valueLivestock, input$flagLivestock),
           c("CPCCode","ElementCode","Year","Value","Flag"))
  data <- data[CPCCode %in% unique(subset(classification, classification %in% c("LP","LD","L"))[,CPCCode])] 
  data <- subset(data, ElementCode %in% c("5510","5318" ,"5319", "5320" ,"5314" ,"5327" ,"5313" ,"5321"))
  data[, Year := as.character(Year)]
  data[, CPCCode := as.character(CPCCode)]
  data[, ElementCode := as.character(ElementCode)]
  data <- data[Year %in% c(input$fromyear : input$endyear)]
  livestock <- data.table(value$data_livestock)
  livestock <- long_format(livestock)
  livestock[,c("Commodity","Element") := NULL]
  livestock[, ElementCode := as.character(ElementCode)]
  xx <- livestock[!is.na(Value)][
    data, on = c("CPCCode", "ElementCode", "Year")]
  xx[, c("Value","Flag"):= NULL]
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  livestock <- livestock[!is.na(Value)][
    !xx,on = c("CPCCode", "ElementCode", "Year")]
  livestock <- rbind(livestock,xx)
  livestock <- merge(livestock, all_elements, by = "ElementCode", all.x = T)
  livestock <- merge(livestock, all_cpc, by= "CPCCode", all.x = T)
  livestock <- livestock[!is.na(Element)]
  livestock <- wide_format(livestock)
  livestock[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_livestock <- livestock
  Add_table_version("livestock", copy(value$data_livestock))
}
  }
     })

observeEvent(input$fileLivestockdenormalized,{
  inFile <- input$fileLivestockdenormalized
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA_live=data.table(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1))
  END_YEAR=input$endyear
  data_denormalized_live <- copy(DATA_live)
  data_denormalized_live <- long_format(data_denormalized_live)
  data_denormalized_live <- data_denormalized_live[CPCCode %in% unique(subset(classification, classification %in% c("LP","LD","L"))[,CPCCode])]
  data_denormalized_live <- data_denormalized_live[Year %in% c(input$fromyear : input$endyear)]
  data_denormalized_live[, c("Commodity","Element") := NULL]
  data_denormalized_live[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
  livestock_Data <- data.table(value$data_livestock)
  livestock_Data <- long_format(livestock_Data)
  livestock_Data[, c("Commodity","Element") := NULL]
  livestock_Data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
  xx=livestock_Data[!is.na(Value)][
    data_denormalized_live,
    on = c("CPCCode", "ElementCode", "Year")]
  xx[, c("Value","Flag"):= NULL]
  setnames(xx,c("i.Value","i.Flag"),c("Value","Flag"))
  livestock_Data <- livestock_Data[
    !xx,
    on = c("CPCCode", "ElementCode", "Year")]
 livestock_Data <- rbind(livestock_Data,xx)
 livestock_Data <- merge(livestock_Data, all_elements, by = "ElementCode", all.x = T)
 livestock_Data <- merge(livestock_Data, all_cpc, by= "CPCCode", all.x = T)
 livestock_Data <- livestock_Data[!is.na(Element)]
 livestock_Data <- subset(livestock_Data, Year %in% 2010:END_YEAR)
 livestock_Data <- wide_format(livestock_Data)
 livestock_Data = visualize_data(livestock_Data,END_YEAR)
 livestock_Data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
 value$data_livestock <- livestock_Data
 Add_table_version("livestock", copy(value$data_livestock))
})

observeEvent(input$saveLivestock,{
  data_to_save <- copy(value$data_livestock)
  data_to_save[,hidden := NULL]
  # write.csv(data_to_save,"test_livestock.csv",row.names = F)
  data_to_save <- subset(data_to_save, ElementCode %in% c("5510","5321","5320","5319"))
  save_to_database(data = data_to_save,year_range = c(input$fromyear:input$endyear))
})

output$livestock <- 
  renderDataTable(
    if(!is.null(value$data_livestock)){
    datatable (value$data_livestock, rownames= FALSE,class = 'cell-border stripe', 
              editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which(colnames(value$data_livestock) == input$fromyear)-2)))), 
              extensions = c("FixedColumns","FixedHeader","Buttons"),
              options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 # dom='f', ordering=F,
                 # paging = TRUE, searching = TRUE, info = FALSE,
                 # sort = TRUE,
                 scrollX = TRUE,
                 scrollY = "500px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(list(width = '150px', targets = c(3)),
                                   list(visible = FALSE, targets = (ncol(value$data_livestock)-1))
                 ) 
               ))  %>%
      formatStyle(0:ncol(value$data_livestock), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    }
 )

}
