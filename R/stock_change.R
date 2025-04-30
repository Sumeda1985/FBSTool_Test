stock_change <- function(input,output,session){
observeEvent(input$undoStock, {
   new_version <- Pop_table_version("stock")  
   if(is.null(new_version)) {
      return()
    }
   value$data_stock <- new_version
 })
  
observeEvent(input$startContinue,{
   value$stockDataLong <- value_database$data[
      ElementCode %in% c("5113","5071") &
        Year %in% c(2010:as.numeric(input$endyear)) &
        StatusFlag == 1,
      .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
    ][!is.na(Value)]
  #updating opening stocks 
      x <- copy(value$stockDataLong[Year %in% c(2014 : input$endyear)])[,c("Commodity","Element","LastModified","StatusFlag") := NULL]
       x <- dcast.data.table(x, CPCCode+ Year ~ ElementCode, value.var = c("Value","Flag"))
      setnames(x, c("Value_5071","Value_5113"),c("delta","new_opening"))
      x[, c("delta", "new_opening") := lapply(.SD, function(v) fifelse(is.na(v), 0, v)), .SDcols = c("delta", "new_opening")]
      x <- x[order(CPCCode, Year)]
      groups <- unique(x[, .(CPCCode)])
      res <- list()
      
      for (i in seq_len(nrow(groups))) {
        z <- x[groups[i], on = c( "CPCCode")]
        # print(i)
        if (nrow(z) > 1) {
          for (j in seq_len(nrow(z))[-1]) {
            
            # print(j)
            # negative delta cannot be more than opening
            if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
              z$delta[j-1] <- - z$new_opening[j-1]
            }
            z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
          }
          # negative delta cannot be more than opening
          if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
            z$delta[j] <- - z$new_opening[j]
          }
        }
        res[[i]] <- z
      }
      data_2013 <- value$stockDataLong[Year %in% c(2010:2013)][,c("LastModified","StatusFlag") := NULL]
      updated_stock <- rbindlist(res)
      ValueData = data.table(melt(updated_stock[,-c("Flag_5071","Flag_5113")], 
                       id.vars = c("CPCCode","Year"),
                       variable.name = "ElementCode", value.name = "Value"))
      ValueData[ElementCode == "delta", ElementCode := "5071"]
      ValueData[ElementCode == "new_opening", ElementCode := "5113"]
      FlagData = data.table(melt(updated_stock[,-c("delta","new_opening")], 
                                 id.vars = c("CPCCode","Year"),
                                 variable.name = "Flag_Des", value.name = "Flag"))[, Flag_Des := NULL]
      update_Stock_final_data <- cbind(ValueData, FlagData[,"Flag"])
      stockData <- rbind(data_2013[,CountryM49 := NULL],update_Stock_final_data)
      stockData <- merge(stockData,all_cpc, by="CPCCode",all.x = T)
      stockData <- merge(stockData,all_elements, by="ElementCode",all.x = T)
      stockData = stockData[order(CPCCode,Year)]
      stockData=wide_format(stockData)
      
      flagcols <- grep("^Flag", names(stockData), value = TRUE)
      yearcols <- grep("^[[:digit:]]{4}$", names(stockData), value = TRUE)
      
      minyear <- min(as.numeric(yearcols))
      maxyear <- max(as.numeric(yearcols))
      
      if(input$endyear > maxyear +1){
        yearsToFill = (maxyear + 1):as.numeric(input$endyear)
        value$data_stock <- NULL
        if(length(yearsToFill) > 0){
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile Stock Change data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
    }
 } else {
        stockData = visualize_data(stockData,input, session)
        stockData <-stockData[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
        stockData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
        value$data_stock <- stockData
        Add_table_version("stock", copy(value$data_stock))
      }
})

#computation of opening stock button 
 observeEvent(input$open_stock_computation, {
    stockData <- long_format(data.table(value$data_stock)[, hidden := NULL])
      x <- copy(stockData[Year %in% c(2014 : input$endyear)])
      x[,c("Commodity","Element") := NULL]
      x <- dcast.data.table(x, CPCCode+ Year ~ ElementCode, value.var = c("Value","Flag"))
      setnames(x, c("Value_5071","Value_5113"),c("delta","new_opening"))
      x[, `:=`(delta = fifelse(is.na(delta), 0, delta),
               new_opening = fifelse(is.na(new_opening), 0, new_opening))][order(CPCCode, Year)]
      groups <- unique(x[, .(CPCCode)])
      res <- list()
      
      for (i in seq_len(nrow(groups))) {
        z <- x[groups[i], on = c( "CPCCode")]
        
        # print(i)
        if (nrow(z) > 1) {
          for (j in seq_len(nrow(z))[-1]) {
            
            # print(j)
            # negative delta cannot be more than opening
            if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
              z$delta[j-1] <- - z$new_opening[j-1]
            }
            z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
          }
          # negative delta cannot be more than opening
          if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
            z$delta[j] <- - z$new_opening[j]
          }
        }
        res[[i]] <- z
      }
      data_2013 <- stockData[Year %in% c(2010:2013)]
      updated_stock <- rbindlist(res)
      ValueData = melt(updated_stock[,-c("Flag_5071","Flag_5113")], 
                       id.vars = c("CPCCode","Year"),
                       variable.name = "ElementCode", value.name = "Value")
      
      ValueData[ElementCode == "delta", ElementCode := "5071"]
      ValueData[ElementCode == "new_opening", ElementCode := "5113"]
      
      FlagData = melt(updated_stock[,-c("delta","new_opening")], 
                      id.vars = c("CPCCode","Year"),
                      variable.name = "Flag_Des", value.name = "Flag")
     FlagData[, Flag_Des := NULL]
      update_Stock_final_data <- cbind(ValueData, FlagData[,"Flag"])
      update_Stock_final_data <- merge(update_Stock_final_data, all_cpc, by = "CPCCode", all.x = TRUE)[
        merge(. , all_elements, by = "ElementCode", all.x = TRUE)]
     stockData <- rbind(data_2013,update_Stock_final_data)[order(CPCCode,Year)]
     stockData=wide_format(stockData)
     # It is needed a validation for opening stock and stock variation. If stock var <0 , abs(stock) < opening stock.
      stock_validation <- copy(stockData)
      stock_validation <- long_format(stock_validation)
      yearval <- c(as.numeric(input$fromyear):as.numeric(input$endyear))
      # print(yearval)
      stock_validation[,validation := ifelse(Year %in% yearval & Value[ElementCode == "5071"]<0 &
                                               Value[ElementCode == "5113"]< abs(Value[ElementCode == "5071"]),1,0),
                       by = c("CPCCode","Year")]
      stock_validation <- stock_validation[validation == 1] 
      if (nrow(stock_validation)>0){
        CommoditiesTocheck <- unique(stock_validation$Commodity)
        sendSweetAlert(
          session = session,
          title = "Issue detected in Stock Variation!!",
          text = paste(c("Please check commodities:", CommoditiesTocheck),collapse= " "),
          type = "Error"
        )
        
      }  else {
      
    stockData <- visualize_data(stockData, input$endyear, session)[
          order(CPCCode, factor(ElementCode, levels = c("5113", "5071"))),
          hidden := as.integer(CPCCode != shift(CPCCode, type = "lead"))
        ]
     value$data_stock <- stockData
   } 
})
observeEvent(input$add_Stock, {
  showModal(viewStockTriplets())
})


viewStockTriplets <- function(failed = FALSE) {
 modalDialog(
   easyClose = TRUE, size= "l",
    dataTableOutput("viewStock"),
    footer = tagList(
    actionButton("stockInsert", "Insert")
    )
  )
}
output$viewStock= renderDataTable({
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    DT=commodity
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    DT <- subset(DT,  !CPCCode %in% unique(value$data_stock[ElementCode == "5071"]$CPCCode) )
    datatable(DT,
            escape=F)
})


observeEvent(input$stockInsert, {removeModal()
})


observeEvent(input$stock_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('stock'),
    cell_edit_info = input$stock_cell_edit,
    data = value$data_stock,
    session = session,
    table_name = "stock"
  )
})

observeEvent(input$stockInsert, {
  handleInsertEvent(
    input_rows = input$viewStock_rows_selected,
    element_codes = c("5113","5071"),
    value_store = value,
    data_type = "stock"
  )
  
})

#delete rows in  table

observeEvent(input$delete_btn_Stock, {
  # Create record of dropped data
  dropstockdata <- value$data_stock[
    as.numeric(input$stock_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5071","5113"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropstockdata)
  value$data_stock <- value$data_stock[!(CPCCode %in% value$data_stock[
    as.numeric(input$stock_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("stock", copy(value$data_stock)) 
})

#download excle file 
output$downloadStock<- createDownloadHandler(reactive(value$data_stock), 
                                             "stock_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileStockdenormalized", 
                         data_key = "data_stock", 
                         version_label =  "stock",
                         value_env = value)
observeEvent(input$uploadStockModal, {
  showModal(uploadStock())
})



uploadStock <- function(failed = FALSE) {
modalDialog(size = "l",titlePanel("Upload File"),
              sidebarLayout(
              sidebarPanel(
                 # Input: Select a file ----
                  fileInput("fileStock", "Choose Excel File",
                            multiple = TRUE,
                            accept = NULL),
                  # tags$script('$( "#fileImport" ).on( "click", function() { this.value = null; });'),
                  selectizeInput("cpcStock", "CPC Code",
                                 selected = NULL, choices = c("",colnames( value$data_stockCountry)),multiple=F),
                  selectizeInput("elementStock", "Element Code",
                                 selected = NULL, choices = c("",colnames( value$data_stockCountry)) ,multiple=F),
                  selectizeInput("yearStock", "Year :",
                                 selected = NULL, choices = c("",colnames( value$data_stockCountry)),multiple=F),
                  selectizeInput("valueStock", "Value :",
                                 selected = NULL, choices = c("",colnames( value$data_stockCountry)),multiple=F),
                  selectizeInput("flagStock", "Flag :",
                                 selected = NULL, choices = c("",colnames( value$data_stockCountry)),multiple=F),
                  actionButton("uploadStock","Upload Stock data")
),
# Main panel for displaying outputs ----
                mainPanel(
                 # Output: Data file ----
                  # dataTableOutput("importCountry")
                  div(style = 'overflow-x: scroll', dataTableOutput('stockCountry'))
                )
                
              )
  )
}

output$stockCountry <- renderDataTable({
  req(input$fileStock)
  inFile <- input$fileStock
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  value$data_stockCountry <- DATA
  datatable(value$data_stockCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})

observe({
 updateSelectInput(session, "cpcStock", choices = c("", colnames( value$data_stockCountry)))
  updateSelectInput(session, "elementStock", choices = c("",colnames( value$data_stockCountry)))
  updateSelectInput(session, "yearStock", choices = c("",colnames( value$data_stockCountry)))
  updateSelectInput(session, "valueStock", choices = c("",colnames( value$data_stockCountry)))
  updateSelectInput(session, "flagStock", choices = c("",colnames( value$data_stockCountry)))
  
})

observeEvent(input$uploadStock, {
  normalized_upload(
    data_source = "data_stockCountry",
    input_cols = c("cpcStock", "elementStock", "yearStock", "valueStock", "flagStock"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("5071", "5113"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_stock",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_stock",
    version_label = "stock",
    session = session,
    input= input
  )
})
#############################################
observeEvent(input$uploadStock, {
removeModal()
})

#save stock
observeEvent(input$saveStock,{
  data_to_save <- copy(value$data_stock)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5071", "5113")]
  # It is needed a validation for opening stock and stock variation. If stock var <0 , abs(stock) < opening stock.
  stock_validation <- long_format(copy(data_to_save))
  yearval <- c(as.numeric(input$fromyear):as.numeric(input$endyear))
  stock_validation[,validation := ifelse(Year %in% yearval & Value[ElementCode == "5071"]<0 &
                                           Value[ElementCode == "5113"]< abs(Value[ElementCode == "5071"]),1,0),
                   by = c("CPCCode","Year")]
 stock_validation <- stock_validation[validation == 1]

 if (nrow(stock_validation)>0){
    CommoditiesTocheck <- unique(stock_validation$Commodity)
    sendSweetAlert(
      session = session,
      title = "Issue detected in Stock Variation!!",
      text = paste(c("Please check commodities:", CommoditiesTocheck),collapse= " "),
      type = "Error"
    )
 }else if (nrow(stock_validation) == 0){
 save_to_database(
     data = data_to_save,
     longData = value$stockDataLong,
     year_range = c(input$fromyear:input$endyear),
     session = session,
     input = input,
     output = output,
     data_session = value$data_stock
   )
    #new_saved_data_stock <- return_data_base(data_to_save)
    #df_sua_unbalanced$data_sua_unbalanced <- new_saved_data_stock
}
})

observeEvent(input$stock_imputation,{
data <- imputeStocksChanges(input,output,session)
data <- wide_format(data)
data <- data[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
if ( is.null(data) ){
sendSweetAlert(
    session = session,
    title = c("Imputation Error"),
    text = c("No time series data to impute"),
    type = "warning"
  )

} else {
  data=visualize_data(data,input, session)
  data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_stock <- data
  Add_table_version("stock", copy(value$data_stock))
}
})

output$stock <-
  renderDataTable(
if (!is.null(value$data_stock)){
    datatable (value$data_stock,rownames= FALSE,class = 'cell-border stripe',
    editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_stock) == input$fromyear)-2)))
                             ),
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 scrollX = TRUE,
                 scrollY = "500px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(
                         list(visible = FALSE, targets = (ncol(value$data_stock)-1))
                 )
   )
)  %>%
formatStyle(0:ncol(value$data_stock), valueColumns = "hidden",
 `border-bottom` = styleEqual(1, "solid 3px")) %>%
formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    }
)
}