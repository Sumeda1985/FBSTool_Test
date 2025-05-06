food=function(input,output,session){
  
observeEvent(input$undoGDP, {
# get last version
new_version <- Pop_table_version("gdp")  
if(is.null(new_version)) {
      return()
}
value$data_gdp <- new_version
})
  
observeEvent(input$undoPopulation, {
# get last version
new_version <- Pop_table_version("popultaion")  
# nothing to reset -- optionally a warning could be displayed in this case
 if(is.null(new_version)) {
      return()
    }
value$data_pop <- new_version
})

observeEvent(input$undoFoodclassific, {
# get last version
    new_version <- Pop_table_version("food_classification")  
# nothing to reset -- optionally a warning could be displayed in this case
if(is.null(new_version)) {
      return()
}
value$data_classification <- new_version
})
observeEvent(input$undoFood, {
# get last version
new_version <- Pop_table_version("food")  
# nothing to reset -- optionally a warning could be displayed in this case
if(is.null(new_version)) {
      return()
}
value$data_food  <- new_version
})
  
##############   GDP Data
observeEvent(input$startContinue,{
    
      data <- data.table(data.table(dbReadTable(con, name="gdpData")))[!duplicated(Year)]
      data[,Year:=as.character(Year)]
if ( input$endyear > max(data$Year)){
 row <- data.table(Year = as.character(input$endyear), 'GDP per capita [constant 2015 US$]' = "")
 data <- rbind(data,row)
}
 else {
  data=data[Year %in% c(2010: as.numeric(input$endyear))]
      }
data[, `GDP per capita [constant 2015 US$]` := round(as.numeric(`GDP per capita [constant 2015 US$]`),0)]
data <- data[order(Year)]
value$data_gdp <- data
Add_table_version("gdp", copy( value$data_gdp))  
})
observeEvent(input$gdp, {
    new <- hot_to_r(input$gdp)
    Add_table_version("gdp", copy(hot_to_r(input$gdp)))
    value$data_gdp <- hot_to_r(input$gdp)
   
  })
  
output$gdp=renderRHandsontable({
    DATA <-value$data_gdp
    t=as.numeric(input$fromyear)
    number_to_freeze=  DATA[!(Year %in% c(2010:as.numeric(t-1))), which =TRUE]
    numeric_columns  <- grep("^[[:digit:]]{4}$", names(DATA), value = TRUE)
    DATA[,(numeric_columns) := round(.SD,0), .SDcols=numeric_columns]
    DATA <- DATA[order(Year)]
    
  if (!is.null(DATA)){
    rhandsontable(DATA[order(Year)],undo = T, redo = T, rowHeaders = NULL,useTypes = T, trimWhitespace =FALSE , Strict = F,readOnly = TRUE,
                  selectCallback = TRUE, fontweight= "bold",search=TRUE,colHeaders = names(DATA))%>%
      hot_cols(format = '0,0')%>%
      hot_col(c("GDP per capita [constant 2015 US$]"),halign = "hLeft",width=190)%>%
      hot_col("Year",halign = "htRight", width = 50)%>%
      hot_row(number_to_freeze, readOnly = FALSE)%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
}  
})
  
observeEvent(input$saveGDP,{
start_year <- as.character(input$fromyear)
data <- data.table(hot_to_r(input$gdp))
data[,Year := as.character(Year)]
gdp_data <- data.table(data.table(dbReadTable(con, name="gdpData")))
gdp_data[,Year := as.character(Year)]
gdp_data <- gdp_data[!duplicated(Year)]
gdp_data <- merge(gdp_data,data,by= "Year",all = TRUE)
gdp_data[, `GDP per capita [constant 2015 US$].x` := ifelse(Year %in% c(start_year:input$endyear), `GDP per capita [constant 2015 US$].y`,
                                                                `GDP per capita [constant 2015 US$].x`)]
gdp_data[, `GDP per capita [constant 2015 US$].y` := NULL]
setnames(gdp_data, "GDP per capita [constant 2015 US$].x","GDP per capita [constant 2015 US$]")
saveRDS(gdp_data,"Data/gdpData.rds")
})
###################  Population Data
observeEvent(input$startContinue,{
   data=data.table(dbReadTable(con, name="pop_sws"))[,timePointYears := as.character(timePointYears)]
   data <- data[,c("timePointYears","Value"),with = F]
   data <-data[timePointYears %in% c(2010:input$endyear)]
   setnames(data, c("timePointYears","Value"),c("Year","Population [1000]"))
   data <- unique(data, by = "Year")[order(Year)]
   if ( input$endyear > max(data$Year)){
      if(as.numeric(max(data$Year))+1 != input$endyear){
          value$data_pop <- NULL
          sendSweetAlert(
            session = session,
            title = "Error!!",
            text = paste("Please compile population data for ",paste(as.numeric(max(data$Year))+1,collapse = ", ") , " first.", sep = ""),
            type = "error"
          )
 } else {
       row <- data.table(Year = as.character(input$endyear), 'Population [1000]' = "")
       data=rbind(data,row)
        }
      }
      else {
        data=data[Year %in% c(2010: as.numeric(input$endyear))]
      }
      data[, `Population [1000]` := round(as.numeric(`Population [1000]`),0)]
      value$data_pop <- data
      Add_table_version("popultaion", copy(value$data_pop))
})
observeEvent(input$popultaion, {
  new <- hot_to_r(input$popultaion)
  # if(any(new != df_pop$data_pop)) {
  Add_table_version("popultaion", copy(hot_to_r(input$popultaion)))
  value$data_pop <- hot_to_r(input$popultaion)
})
output$popultaion=renderRHandsontable({
    t=as.numeric(input$fromyear)
    number_to_freeze <- value$data_pop[!(Year %in% c(2010:as.numeric(t-1))), which =TRUE]
    numeric_columns  <- grep("^[[:digit:]]{4}$", names(value$data_pop), value = TRUE)
    value$data_pop[,(numeric_columns) := round(.SD,0), .SDcols=numeric_columns]
    if (!is.null(value$data_pop)){
     rhandsontable(value$data_pop,undo = T, redo = T,rowHeaders = NULL, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,readOnly = TRUE,
                    selectCallback = TRUE, fontweight= "bold",search=TRUE,colHeaders = names(value$data_pop))%>%
        hot_cols(format = '0,0')%>%
        hot_col(c("Population [1000]"),halign = "hLeft",width=190)%>%
        hot_col("Year",halign = "htRight", width = 50)%>%
        hot_row(number_to_freeze, readOnly = FALSE)%>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
}  
})
observeEvent(input$savePopulation,{
  data <- data.table(hot_to_r(input$popultaion))[,Year := as.character(Year)]
  popData <- data.table(dbReadTable(con, name="pop_sws"))
  popData[, timePointYears := as.character(timePointYears)]
  popData <- popData[!duplicated(timePointYears)]
  data <- merge(popData,data, by.x = c("timePointYears"),by.y = c("Year"),all = TRUE)
  data[, Value := ifelse(timePointYears %in% c(input$fromyear:input$endyear), `Population [1000]`,Value )]
  data[, `Population [1000]` := NULL]
  data[is.na(geographicAreaM49), geographicAreaM49 := unique(countryData$CountryM49)]
  data[is.na(measuredElement), measuredElement := "511"]
  data[is.na(flagObservationStatus), flagObservationStatus := "X"]
  data[is.na(flagMethod), flagMethod := "h"]
  saveRDS(data,"SUA-FBS Balancing/Data/popSWS.rds")
 })
###################################  Food Demand Model ###################################################
observeEvent(input$startContinue,{
     data <- data.table(dbReadTable(concore, name= "food_demand" ))
     value$data_fdm <- data
})  
observeEvent(input$saveFDM,{
  data <- data.table(hot_to_r(input$food_fdm))
  saveRDS(data, "Data/fdmData.rds")
})
  
output$food_fdm=renderRHandsontable({
 rhandsontable(value$data_fdm,height = 400,undo = T, redo = T,rowHeaders = NULL,readOnly =T,
                  useTypes = T,trimWhitespace =FALSE , Strict = F, columnSorting = TRUE, copy=T,paste=T
                  , selectCallback = TRUE, fontweight= "bold",search= TRUE,colHeaders = names(value$data_fdm))%>%
      hot_col("CPCCode", halign = "htRight", width = 100)%>%
      hot_col("Commodity", halign = "htLeft", width = 200)%>%
      hot_col("Elasticity", halign = "htRight", width = 100)%>%
      hot_col("FBSCode", halign = "htRight", width = 100)%>%
      hot_col("Food Demand", halign = "htRight", width = 100)%>%
      hot_col("Food Function", halign = "htRight", width = 100)%>%
      hot_col("Description", halign = "htLeft", width = 100)%>%
      hot_col("FBSCommodity", halign = "htLeft", width = 250)%>%
      
      #   # hot_cols(format = '0,0')%>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
      hot_col("FBSCommodity", width= 250)%>%
    hot_cols(fixedColumnsLeft = 6) 
  })
############################  Food Classification Table #######################################################
observeEvent(input$startContinue,{
    data <- data.table(dbReadTable(con, name="food_classification"))[!duplicated(CPCCode)]
    value$data_classification <- data
    Add_table_version("food_classification", copy(value$data_classification))
})
observeEvent(input$savefoodclassific,{
   DATA <- hot_to_r(input$food_classification)
   DATA <- data.table(DATA)
   saveRDS(DATA,"Data/foodCommodityList.rds")
  })
  
  observeEvent(input$food_classification, {
    new <- hot_to_r(input$food_classification)
    new %>% mutate(Type = as.character(Type))
    # convert type to character as comparison is not implemented for ordered factors
    if(any(new %>% mutate(Type = as.character(Type)) != 
       value$data_classification %>% mutate(Type = as.character(Type)))) {
      Add_table_version("food_classification", copy(new))
      value$data_classification <- new
    }
})
output$food_classification=renderRHandsontable({
    DATA <-value$data_classification
     DATA[, Type := as.factor(Type)]
if (!is.null(DATA)){
 output <- rhandsontable(DATA,undo = T, redo = T,rowHeaders = NULL, useTypes = T, trimWhitespace =FALSE , Strict = F, columnSorting = TRUE,readOnly = FALSE,
                    selectCallback = TRUE, fontweight= "bold",search=TRUE,colHeaders = names(DATA))%>%
        hot_col(c("CPCCode","Commodity"), readOnly = T)
}  
})
#################################################################  Food Data #######################################################################  
observeEvent(input$startContinue,{
  value$foodDataLong <- value_database$data[
    ElementCode %in% c("5141") &
      Year %in% c(2010:as.numeric(input$endyear)) &
      StatusFlag == 1,
    .(CountryM49, CPCCode, ElementCode, Year, Flag, LastModified, StatusFlag, Value)
  ][!is.na(Value)]
   foodData <- value_database$data[ElementCode %in% c("5141") & StatusFlag == 1
                      & Year %in% c(2010:input$endyear)][!is.na(Value)]
  foodData <- wide_format(foodData)
 # Get column information
  yearcols <- grep("^[[:digit:]]{4}$", names(foodData), value = TRUE)
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  if(input$endyear > maxyear + 1){
    yearsToFill <- (maxyear + 1):as.numeric(input$endyear)
    value$data_food <- NULL
    if(length(yearsToFill) > 0){
      sendSweetAlert(
        session = session,
        title = "Error!!",
        text = paste("Please compile Food data for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
        type = "error"
      )
    }
  } else {
    foodData = visualize_data(foodData,input, session)
    foodData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_food <- foodData
    Add_table_version("food", copy(value$data_food))  
  }
  })
observeEvent(input$add_Food, {
  showModal(viewFoodTriplets())
})
viewFoodTriplets <- function(failed = FALSE) {
   modalDialog(easyClose = TRUE, size= "l",dataTableOutput("viewFood"),
   footer = tagList( actionButton("foodInsert", "Insert")
      )
    )
 }
 output$viewFood= renderDataTable({
    commodity=copy(all_cpc)
    commodity <- commodity[order(CPCCode),]
    commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
    commodity <- commodity[!CPCCode %in% unique(value$data_food$CPCCode)]
    value$foodCommodities <- commodity
    datatable(value$foodCommodities,escape=F)
})
observeEvent(input$foodInsert, {
    removeModal()
 })
observeEvent(input$food_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('food'),
    cell_edit_info = input$food_cell_edit,
    data = value$data_food,
    session = session,
    table_name = "food"
  )
})
observeEvent(input$foodInsert, {
  handleInsertEvent(
    input_rows = input$viewFood_rows_selected,
    element_codes = c("5141"),
    value_store = value,
    data_type = "food"
  )
})
#delete rows in food table
observeEvent(input$delete_btn_food, {
  # Create record of dropped data
  dropfooddata <- value$data_food[
    as.numeric(input$food_rows_selected),
    .(
      CountryM49 = countrycode(input$countrym49, origin = 'country.name', destination = 'un'),
      Country = input$countrym49,
      CPCCode,
      ElementCode = c("5141"),
      StatusFlag = 0
    )
  ]
  # Update dropped data record and remove from main table
  value$dropdata <- rbind(value$dropdata, dropfooddata)
  value$data_food <- value$data_food[!(CPCCode %in% value$data_food[
    as.numeric(input$food_rows_selected), unique(CPCCode)
  ])]
  Add_table_version("food", copy(value$data_food)) 
})
#download excle file (#downloadCrop)
output$downloadFood<- createDownloadHandler(reactive(value$data_food),"food_data.xlsx")
upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileFooddenormalized", 
                         data_key = "data_food", 
                         version_label =  "food",
                         value_env = value)
observeEvent(input$uploadFoodModal, {
    showModal(uploadFood())
})
uploadFood <- function(failed = FALSE) {
  modalDialog(size = "l",
         titlePanel("Upload File"),
         sidebarLayout(
         sidebarPanel(
         fileInput("fileFood", "Choose Excel File",  multiple = TRUE, accept = NULL),
         selectizeInput("cpcFood", "CPC Code",
                                   selected = NULL, choices = c("",colnames( value$data_foodCountry)),multiple=F),
         selectizeInput("elementFood", "Element Code",
                                   selected = NULL, choices = c("",colnames( value$data_foodCountry)) ,multiple=F),
         selectizeInput("yearFood", "Year :",
                                   selected = NULL, choices = c("",colnames( value$data_foodCountry)),multiple=F),
         selectizeInput("valueFood", "Value :",
                                   selected = NULL, choices = c("",colnames( value$data_foodCountry)),multiple=F),
         selectizeInput("flagFood", "Flag :",
                                   selected = NULL, choices = c("",colnames( value$data_foodCountry)),multiple=F),
         actionButton("uploadFood","Upload Food data")
          ),
         mainPanel(
           div(style = 'overflow-x: scroll', dataTableOutput('foodCountry'))))
    )
 }
output$foodCountry <- renderDataTable({
  req(input$fileFood)
  inFile <- input$fileFood
  file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
   DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
   value$data_foodCountry <- DATA
   datatable(value$data_foodCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
})
observe({
    updateSelectInput(session, "cpcFood", choices = c("", colnames( value$data_foodCountry)))
    updateSelectInput(session, "elementFood", choices = c("",colnames( value$data_foodCountry)))
    updateSelectInput(session, "yearFood", choices = c("",colnames( value$data_foodCountry)))
    updateSelectInput(session, "valueFood", choices = c("",colnames( value$data_foodCountry)))
    updateSelectInput(session, "flagFood", choices = c("",colnames( value$data_foodCountry)))
})
observeEvent(input$uploadFood, {
    removeModal()
 })
  
observeEvent(input$uploadFood, {
  normalized_upload(
    data_source = "data_foodCountry",
    input_cols = c("cpcFood", "elementFood", "yearFood", "valueFood", "flagFood"),
    std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
    filter_element_code = c("5141"),
    from_year = input$fromyear,
    to_year = input$endyear,
    existing_data = "data_food",
    all_elements = all_elements,
    all_cpc = all_cpc,
    value_env = value,
    value_name = "data_food",
    version_label = "food",
    session = session,
    input=input
  )
}) 
  
observeEvent(input$saveFood, {
  # Prepare data for saving
  data_to_save <- copy(value$data_food)
  data_to_save[, hidden := NULL]
  data_to_save <- data_to_save[ElementCode %in% c("5141")]
  # Save to database
  save_to_database(
    data = data_to_save,
    longData = value$foodDataLong ,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_food
  )
})

observeEvent(input$food_imputation,{
   gdp_Data <- data.table(data.table(dbReadTable(con, name="gdpData")))
   year_Range <- c(input$fromyear : input$endyear) 
   timeseries <-data.table(expand.grid(Year = as.character(year_Range)))
   timeseries <- merge(timeseries, gdp_Data, all.x = TRUE)
   empty_cell <- unique(timeseries[Year %in% year_Range]$`GDP per capita [constant 2015 US$]`)
   if (NA %in% empty_cell){
      sendSweetAlert(
        session = session,
        title = "Missing GDP Data !!",
        text = "Some GDP per capita are missing",
        type = "error"
      )
} else {
    data <- imputeFood(input,output,session)
    sendSweetAlert(
        session = session,
        title = "Imputed !!",
        text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
        type = "success"
      )
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
      value$data_food <- data
      Add_table_version("food", copy( value$data_food)) 
}
} 
})
output$food <- 
    renderDataTable(
      if (!is.null(value$data_food)){
        datatable (value$data_food, rownames= FALSE,class = 'cell-border stripe', 
        editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_food) == input$fromyear)-2)))),
        extensions = c("FixedColumns","FixedHeader","Buttons"),
         options = list(
            pageLength = 25,
            dom= 'Blfrtip', buttons = I('colvis'),
            scrollX = TRUE,
            scrollY = "500px" ,
            autoWidth = T,
            fixedColumns = list(leftColumns = 4),
            columnDefs = list(
              list(visible = FALSE, targets = (ncol(value$data_food)-1))
                     )
                   ))  %>%
          formatStyle(0:ncol(value$data_food), valueColumns = "hidden",
                      `border-bottom` = styleEqual(1, "solid 3px")) %>%
          formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
      }
 )
}
