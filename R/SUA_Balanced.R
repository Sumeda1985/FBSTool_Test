SUA_Balanced <- function(input,output,session){
  
suaBal_NU <- eventReactive(c(input$checkbox,input$gotosuaBalanced),{
t=as.numeric(input$endyear)
files_sua_balanced=list.files(path = "SUA-FBS Balancing/Data",pattern ="^sua_balanced")
#if (paste0("sua_balanced.rds") %in% files_sua_balanced){
   data <- value$sua_balanced_plugin
   data[, c("geographicAreaM49", "measuredElementSuaFbs","timePointYears") := lapply(.SD, as.character), 
           .SDcols = c("geographicAreaM49", "measuredElementSuaFbs", "timePointYears")]
   data <- subset(data, timePointYears %in% c(2014:t))
   #Attach 2010-2013 SUA BAlanced data
   #removed as instructed by Vikas
   #sua_bal_2010_2013 <- fread_rds("SUA-FBS Balancing/Data/sua_bal_2010_2013.rds")[timePointYears %in% c(2010:2013)]
   #data <- rbind(data,sua_bal_2010_2013)
   #remove variables where there exists only 5166 for a particular CPC
   remove_row <- unique(data[,c("measuredItemFbsSua","measuredElementSuaFbs"),with = F])
   duplicated_Row <- remove_row[duplicated(measuredItemFbsSua)]
   remove_row <- remove_row[!measuredItemFbsSua %in% unique(duplicated_Row$measuredItemFbsSua)]
   data <- subset(data,! measuredItemFbsSua %in% unique(remove_row$measuredItemFbsSua))
   #These data does not consist tourist if the year is before or equal to 2013
   data[, geographicAreaM49 := NULL]
   setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
               c("ElementCode","CPCCode","Year","Value","Flag"))
   data[, ElementCode := as.character(ElementCode)]
   data=merge(data, all_cpc, by= "CPCCode", all.x = TRUE)
   data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
   # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
      #tourist remove
    data <- data[!is.na(Commodity)]
    elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141","664","665", "674","684","5166","5113"
                 ,new_nutrient_element)
    data <- data[ElementCode %in% elemKeys]
    # data[,Value := round(Value,0)]
    data <- wide_format(data)
    fbsTree <- data.table(dbReadTable(concore,"fbs_tree"))
    fbsTree <- fbsTree[, c("item_sua_fbs","id4")]
    setnames(fbsTree, c("item_sua_fbs","id4"),c("CPCCode","FBS Code"))
    fbsTree[, fbsCode_S := paste0("S",`FBS Code`)]
    commodityName <- data.table(dbReadTable(concore,"SUA_Commodities"))
    fbsTree <- merge(fbsTree,commodityName, by.x = "fbsCode_S", by.y = "CPCCode", all.x = TRUE)
    fbsTree[, fbsCode_S := NULL]
    setnames(fbsTree,"Commodity","FBS Commodity")
    data <- merge(data,fbsTree, by= c("CPCCode"),all.x = TRUE)
    year_col <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    flag_col <- grep("^Flag", names(data), value = TRUE)
    addorder <- as.vector(rbind(year_col,flag_col))
    setcolorder(data,c("FBS Code", "FBS Commodity", "CPCCode","Commodity","ElementCode","Element",addorder))
    data[, `FBS Code` := as.character(`FBS Code`)]
    data <- data[with(data, order(`FBS Code`, CPCCode,factor(ElementCode, levels = c("5113", "5510","5610","5910"
                                              ,"5071", "5141", "5525","5520",
                                              "5016","5023","5165","5166","664","665","674","684",
                                              new_nutrient_element)))), ]
    value$data_sua_balanced_with_nut <- data[!ElementCode %in% c("4030","4031","4023","4014","4008")] #remove ash ,water et
    if (input$checkbox == FALSE ){
        data <- data[!ElementCode %in% c("674","684",new_nutrient_element)]
        data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
        value$data_sua_balanced <- data
        
      }else if (input$checkbox == TRUE) {
        value$data_sua_balanced <- data
        data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
      }
    #}
#else {
     # sendSweetAlert(
       # session = session,
        #title = paste("Please Run the Balancing Plugin to create SUA Balanced of", t),
        # text = paste(elementNameMissing, collapse = ' , '),
        #type = "warning"
      #)
  #}
 })

output$download_sua_balanced<- downloadHandler(
  filename = function() {
    "SUA_Balanced_data.xlsx"
  },
  content = function(file) {
    data_download <- data.table(value$data_sua_balanced)
    data_download <- data_download[!is.na(CPCCode)]
    data_download[,hidden := NULL]
    write.xlsx(data_download ,file,row.names = FALSE)
  }
)

observeEvent(input$startContinue, {
   data <- data.table(dbReadTable(con, "nutrient_data"))[StatusFlag == 1]
   data[, c("StatusFlag","LastModified") := NULL]
   #data=fread_rds("SUA-FBS Balancing/Data/nutrientData.rds")
   t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
   data <- data[!duplicated(data)]
   data=data[timePointYearsSP %in% t]
   data[,c("flagRatio", "geographicAreaM49"):=NULL]
   setnames(data,c("measuredItemCPC", "measuredElement"),c("CPCCode","ElementCode"))
   data[, ElementCode := as.character(ElementCode)]
   data=merge(data,all_cpc,by="CPCCode", all.x = TRUE)
   data=merge(data,all_elements,by="ElementCode", all.x = TRUE)
   setnames(data,"timePointYearsSP","Year")
   setcolorder(data,c("CPCCode","Commodity","ElementCode","Element","Year","Value"))
   value$data_nutrient <- data
})
proxy_nutrient_factors = dataTableProxy('nutrient_factors')
observeEvent(input$nutrient_factors_cell_edit, {
  info = input$nutrient_factors_cell_edit
  print(info)
  i = info$row
  j = (info$col + 1)
  v = info$value
  value$data_nutrient[i,(j) := v]
  replaceData(proxy_nutrient_factors, value$data_nutrient, resetPaging = FALSE,rownames = FALSE)  # important
})
output$download_nutri_table<- downloadHandler(
  filename = function() {
    "Nutrient Factors.xlsx"
  },
 content = function(file) {
 data_download <- data.table(value$data_nutrient)
 write.xlsx(data_download ,file,row.names = FALSE)
  }
)

observeEvent(input$nutrientSave,{
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  nutrientOriginal <- fread_rds("SUA-FBS Balancing/Data/nutrientData.rds")
  nutrient <- data.table(value$data_nutrient)
  nutrient[,c("Commodity","Element") := NULL]
  setnames(nutrient, c("CPCCode","ElementCode","Year", "Value"),c("measuredItemCPC","measuredElement",
                                                                  "timePointYearsSP", "new_value"))
  nutrient[,measuredElement := as.character(measuredElement)]
  nutrientOriginal[,measuredElement := as.character(measuredElement)]
   nutrient_to_save <- merge(nutrientOriginal,nutrient, by=c("measuredItemCPC","measuredElement","timePointYearsSP"), 
                            all.x = T )
  nutrient_to_save[ timePointYearsSP  %in% t,
                    `:=` (Value = new_value
                    )]
  nutrient_to_save <- nutrient_to_save[,c("new_value") := NULL]
  saveRDS(nutrient_to_save,"SUA-FBS Balancing/Data/nutrientData.rds")
  
})
output$nutrient_factors <- 
  renderDataTable(
    datatable(value$data_nutrient, rownames= FALSE,class = 'cell-border stripe',
              editable = list(target = "cell", disable = list(columns = c(1:4))), 
              options = list(columnDefs = list(list(width = '40px', targets = c(0,1,2,3,4,5)))))
 )

output$sua_balanced <-
  renderDataTable({
    if (is.null(suaBal_NU())){
      validate(
        need(nrow(suaBal_NU())>0, "Please run the balancing Plugin")
      )
    }
    datatable (suaBal_NU(), rownames= FALSE,class = 'cell-border stripe',
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 # fixedHeader= TRUE,
                 scrollX = TRUE,
                 scrollY = "400px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 6),
                 columnDefs = list(
                   list(visible = FALSE, targets = (ncol(suaBal_NU())-1)))
               ))  %>%
      formatStyle(0:ncol(suaBal_NU()), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px"))%>%
      formatStyle('ElementCode', target = "row", color = styleEqual(unique(suaBal_NU()$ElementCode),
                                                                    ifelse(unique(suaBal_NU()$ElementCode)%in% c('5166','664','665'),'blue','black')))%>%
      formatCurrency(columns = as.character(c(2014:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
})

 
  
}