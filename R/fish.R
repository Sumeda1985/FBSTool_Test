fish <- function(input,output,session){
observeEvent(input$undoFish, {
    # get last version
    new_version <- Pop_table_version("fish")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    value$data_fish <- new_version
 })
  
 observeEvent(input$startContinue,{
   fishData=fread_rds("Data/fish.rds")
   fishData <- subset(fishData, Year %in% c(2010 : input$endyear) )
   setnames(fishData,c("Element Code", "Item Code (CPC)"),c("ElementCode", "CPCCode"))
   fishData[,ElementCode := as.character(ElementCode)]
   fishData <- merge(fishData,all_elements, by = "ElementCode",all.x = TRUE)
   fishData[,Commodity:= "Fish, Seafood"]
   fishData=wide_format(fishData)
   flagcols <- grep("^Flag", names(fishData), value = TRUE)
   yearcols <- grep("^[[:digit:]]{4}$", names(fishData), value = TRUE)
   minyear <- min(as.numeric(yearcols))
    maxyear <- max(as.numeric(yearcols))
   if(input$endyear > maxyear +1){
     yearsToFill = (maxyear + 1):as.numeric(input$endyear)
      value$data_fish <- NULL
      if(length(yearsToFill) > 0){
        sendSweetAlert(
          session = session,
          title = "Error!!",
          text = paste("Please compile Fish data for the year(s)",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
          type = "error"
        )
   }
  } else {
      fishData = visualize_data(fishData,input, session)
      fishData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
      value$data_fish <- fishData
      Add_table_version("fish", copy(value$data_fish))
 }
    
})
 
 observeEvent(input$fish_cell_edit, {
   handle_cell_edit(
     proxy = dataTableProxy('fish'),
     cell_edit_info = input$fish_cell_edit,
     data = value$data_fish,
     session = session,
     table_name = "fish"
   )
 })
 
observeEvent(input$saveFish,{
    data_to_save <- value$data_fish
    data_to_save <- long_format(data_to_save)
    data_to_save[, c("Commodity", "Element") := NULL]
    setnames(data_to_save,c("ElementCode", "CPCCode"),c("Element Code", "Item Code (CPC)"))
    saveRDS(data_to_save,"Data/fish.rds")
})
  
output$fish <- 
    renderDataTable(
     if (!is.null(value$data_fish)){
        datatable (value$data_fish, rownames= FALSE,class = 'cell-border stripe', 
             editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_fish) == input$fromyear)-2)))),
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
                     columnDefs = list(
                       #list(width = '150px', targets = c(3))
                       # no hide column
                       list(visible = FALSE, targets = (ncol(value$data_fish)-1))
                     )
                   ))  %>%
          formatStyle(0:ncol(value$data_fish), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
)
  
  
}