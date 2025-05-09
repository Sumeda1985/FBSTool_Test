FBS_Balanced <- function(input,output,session){
observeEvent(input$fbs_balanced_button,{
show_modal_spinner(
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
)
fbs_balanced_plugin(input,output,session)
Sys.sleep(6)
remove_modal_spinner()
t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
#files_fbs_balanced=list.files(path = "SUA-FBS Balancing/FBS_Balanced/Data",pattern ="^fbs_balanced_final.rds")
#if (paste0("fbs_balanced_final.rds") %in% files_fbs_balanced){
     #data=fread_rds("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.rds")
     data <- value$fbs_balanced_plugin
     data <- data[timePointYears %in% t]
     data[, geographicAreaM49 := NULL]
     setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
              c("ElementCode","CPCCode","Year","Value","Flag"))
     data[, c("ElementCode", "Year") := lapply(.SD, as.character), .SDcols = c("ElementCode", "Year")]
     data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
     commodityName <- data.table(dbReadTable(concore, name="SUA_Commodities"))
     data <-merge(data,commodityName,by = "CPCCode",all.x = TRUE)
     data <- data[!is.na(Commodity)]
     elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5141","5166")
     data=subset(data, ElementCode %in% elemKeys)
     data[,Value := round(Value,0)]
     data <- data[!CPCCode %in% c("2910")]
     data <- wide_format(data)
     setnames(data,c("CPCCode","Commodity"), c("FBS Code","FBS Group"))
     data[, `FBS Code` := as.character(`FBS Code`)]
     data <-data[order(`FBS Code`, factor(ElementCode, levels = c("5510","5610","5910","5071", "5141", "5525","5520","5016","5023","5165","5166",
                                                                   "664","665","674","684","261","271","281")))]
     data[, hidden := ifelse(`FBS Code` != shift(`FBS Code`, type = "lead"), 1, 0)] 
     value$data_fbs_balanced <- data
     newtab <- switch(input$fao,
                       "sua_balanced" = "fbs"
                       
      )
      updateTabItems(session, "fao", newtab)
   # }else {
      #sendSweetAlert(
       # session = session,
       # title = paste("Please Run the Balancing Plugin to create FBS Balanced of", t),
        # text = paste(elementNameMissing, collapse = ' , '),
       # type = "warning"
      #)
  #}
 })
  
observeEvent(input$total_DES,{
t= as.numeric(input$endyear)
#files_fbs_balanced=list.files(path = "SUA-FBS Balancing/FBS_Balanced/Data",pattern ="^fbs_balanced_final.rds")
 #if (paste0("fbs_balanced_final.rds") %in% files_fbs_balanced){
      #data=fread_rds("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.rds")
      data <- value$fbs_balanced_plugin
      #####fishery calories must be binned here      ##new
      fish <- data.table(dbReadTable(con, name="fish"))[,StatusFlag := 1]
      fish[,c("StatusFlag","LastModified") := NULL]
      setnames(fish,c("Item.Code..CPC.","Element.Code","Year","Flag"),c("measuredItemFbsSua", "measuredElementSuaFbs","timePointYears", "flagObservationStatus"))
      fish[,geographicAreaM49 := unique(data$geographicAreaM49)]  
      data <- rbind(data,fish)
      #########################################################   ## new 
      data <- subset(data, timePointYears %in% c(2014:t)) #new
      data[, geographicAreaM49 := NULL]
      setnames(data,c("measuredElementSuaFbs", "measuredItemFbsSua", "timePointYears","Value", "flagObservationStatus"),
               c("ElementCode","CPCCode","Year","Value","Flag"))
      data[, c("ElementCode", "Year") := lapply(.SD, as.character), .SDcols = c("ElementCode", "Year")]
      ### before merging nutrient factors  
      replacements = data.table(
        ele1  = unique(nutrientEle$ElementCode), 
        ele2 = unique(nutrientEle$measuredElement)
      )
      data <- merge(data,replacements, by.x = "ElementCode", by.y = "ele2", all.x = TRUE)
      data[, ElementCode := ifelse(!is.na(ele1),  ele1, ElementCode)][, ele1 := NULL]
     #request by Rachele to remove Ash, water , alcohol and Nianacin, food quantity
      data <- data[!ElementCode %in% c("4031","4030","4008","4023", "4014", "4001", "665")]
      ########################################
      data=merge(data,all_elements, by="ElementCode", all.x = TRUE)
      commodityName <- data.table(dbReadTable(concore, "SUA_Commodities"))
      data <-merge(data,commodityName,by = "CPCCode",all.x = TRUE)
      # elemKeys=c("5510", "5610", "5071", "5023", "5910", "5016", "5165", "5520","5525","5164","5141")
      #tourist remove
      data <- data[!is.na(Commodity)]
      # elemKeys=c("664", "665","674","684","261","271","281",new_nutrient_element)
      elemKeys=c("664","674","684",new_nutrient_element) # removed 261,271, 281,665
      data=subset(data, ElementCode %in% elemKeys)
      ##################### new ############### fishery inclusion ####################### ## new 
      ##2901 grand Total (Including Fishery)
      ##2902 grand total (excluding Fishery)
      tempdata <- data[CPCCode %in% c("S2901", "S2960") & ElementCode %in% c("664","674","684",new_nutrient_element)][,c("CPCCode","ElementCode","Year","Value"), with = F] 
      tempdata[, new := sum(Value[CPCCode %in% "S2901"], Value[CPCCode %in% "S2960"],na.rm = TRUE),.(Year,ElementCode)]
      tempdata <- unique(tempdata[,c("ElementCode","Year","new"),with =F])
      tempdata[, CPCCode := "S2901"]
      setnames(tempdata,"new","Value")
      tempdata[,Flag := "I"]
      tempdata[, Commodity := "Grand Total (incl. Fish, Seafood)"]
      tempdata <- merge(tempdata,all_elements, by = "ElementCode", all.x = TRUE)
      ## 2901 must be converted to 2902 as grand total (excluding fishery) but only for calories , fats and proteins 664, 674, 684 
      #(2901 code will be there for 261,271,281)
      data[CPCCode == "S2901" & ElementCode %in% c("664","674","684"),  ':=' (CPCCode ="S2902", Commodity = "Grand Total (excl. Fish, Seafood)")]
      ###################################################################################    
      data <- rbind(data,tempdata)
      ###add new nutrient at total level
      NutriTotData <- copy(data)[CPCCode %in% c("S2903","S2941") & ElementCode %in% new_nutrient_element]
      NutriTotData <- aggregate(
        Value ~ ElementCode+Year, NutriTotData,
        sum, na.rm = TRUE)
      NutriTotData <- data.table(NutriTotData)
      NutriTotData[, `:=`(Flag = "I", CPCCode = "S2901", Commodity = "GRAND TOTAL - DEMAND")]
      NutriTotData <- merge(NutriTotData, all_elements, by = "ElementCode", all.x = TRUE)
      data <- rbind(data,NutriTotData)
      # data[,Value := round(Value,0)]
      # data <- wide_format(data)
      setnames(data,c("CPCCode","Commodity"), c("FBS Code","FBS Group"))
      data[, `FBS Code` := as.character(`FBS Code`)]
      data[, Flag := NULL]
      data[,`FBS Code` := sub('.', '', `FBS Code`)]
      data =  dcast.data.table(data, `FBS Code`+`FBS Group`+ElementCode+Element ~ Year, value.var = c("Value"))
      # data <- data[ElementCode %in% "664"]
      fbsTree= data.table(dbReadTable(concore, "fbs_tree"))
      ##new
      #spliiting the rows  ## this is including fish 
      rbind1= data[`FBS Code`== "2901"]
      rbind1 <- rbind1[order(`FBS Code`, factor(ElementCode, levels = c("664","665", "674","684","261","271","281",new_nutrient_element)))]
      ##new total excluding fish
      rbind_ex_fish= data[`FBS Code`== "2902"]
      rbind_ex_fish <- rbind_ex_fish[order(`FBS Code`, factor(ElementCode, levels = c("664", "674","684",new_nutrient_element)))]
      #fish  #new
      rbind_fish = data[`FBS Code`== "2960"]
      rbind_fish <- rbind_fish[order(`FBS Code`, factor(ElementCode, levels = c("664", "674","684")))]
      #Vegetable products
      rbind2=data[`FBS Code`== "2903"]
      rbind2 <- rbind2[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
      DT_vegetables <- copy(data)
      DT_vegetables<-DT_vegetables[0,]
      for (i in sort(unique(fbsTree[id2== "2903"]$id3),decreasing = FALSE)){
        level_3 = data[`FBS Code` == i]
        level_3 <- level_3[order(`FBS Code`, factor(ElementCode, levels = c("664","665", "674","684","261","271","281",new_nutrient_element)))]
        level_4= data[`FBS Code` %in%  unique( fbsTree[id3 == i ]$id4)]
        level_4 <- level_4[order(`FBS Code`, factor(ElementCode, levels = c("664", "665","674","684","261","271","281",new_nutrient_element)))]
        DT_vegetables=rbind(DT_vegetables,level_3,level_4)
      }
      #Animal Products
      rbind3=data[`FBS Code`== "2941"]
      rbind3 <- rbind3[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
      DT_animalproducts<- copy(data)
      DT_animalproducts <- DT_animalproducts[0,]
      for (i in sort(unique(fbsTree[id2== "2941"]$id3),decreasing = FALSE)){
        level_3_animal = data[`FBS Code`== i]
        level_3_animal <- level_3_animal[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
        level_4_animal= data[`FBS Code` %in%  unique( fbsTree[id3 == i ]$id4)]
        level_4_animal <- level_4_animal[order(`FBS Code`, factor(ElementCode, levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
        DT_animalproducts=rbind(DT_animalproducts,level_3_animal,level_4_animal)
      }
      DESCPC_final=rbind(rbind1,rbind_ex_fish,rbind_fish,rbind2,DT_vegetables,rbind3, DT_animalproducts)
      DESCPC_final[, hidden := ifelse(`FBS Code` != shift(`FBS Code`, type = "lead"), 1, 0)] 
      value$data_des <- DESCPC_final
     #}else {
     # sendSweetAlert(
      #  session = session,
       # title = paste("Please Run the Balancing Plugin to create Total Calories of", t),
        # text = paste(elementNameMissing, collapse = ' , '),
       # type = "warning"
      #)
   # }
})
output$download_fbs_balanced<- downloadHandler(
   filename = function() {
      "FBS_Balanced_by_year.xlsx"
    },
    content = function(filename) {
      data_full <- value$fbs_balanced_plugin
      replacements = data.frame(
        ele1  = unique(nutrientEle$ElementCode), 
        ele2 = as.character(unique(nutrientEle$measuredElement))
      )
      data_full <- merge(data_full,
                         replacements, by.x = "measuredElementSuaFbs", by.y = "ele2", all.x = TRUE)
      nutrient_data <- data_full %>%
        filter(!is.na(ele1)) %>%
        rename(`year` = timePointYears,
               `FBS Code` = measuredItemFbsSua,
               `ElementCode` = ele1,
               value = Value) %>%
        dplyr::select(`FBS Code`, ElementCode, year, value)
       require(openxlsx)
      wb <- createWorkbook()
      for(year in as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))) {
        data_sheet = Prepare_data_fbs_report(
          year = year,
          fbs_tree = data.table(dbReadTable(concore, "fbs_tree")),
          fbs_balanced = value$data_fbs_balanced,
          fbs_balanced_final = value$fbs_balanced_plugin,
          commodity_names = data.table(dbReadTable(concore, "SUA_Commodities")),
          nutrient_data = nutrient_data,
          population_data = data.table(dbReadTable(con, "pop_sws"))[StatusFlag== 1][,c("StatusFlag","LastModified") := NULL],
          elements = data.table(dbReadTable(concore, "elements")),
          country = value_database$data$Country[1]
        )
       Add_FBS_year_sheet_formatted2(wb, as.character(year), data_sheet)
      }
      
   saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    }
  )  
 
# update select year/nutrient when data des is loaded
observe({
  data <- value$data_des
  if(is.null(data)) {
    return()
  }
  # set available years
  cols <- colnames(data)
  year_col <- !grepl("\\D", cols)
  years <- cols[year_col]
  updateSelectInput(session, 'select_des_data_year', choices = years)
  # set available nutrients
  nutrients <- unique(data$Element)
  updateSelectInput(session, 'select_des_data_nutrient', choices = nutrients)
})

des_table_data <- reactive({
  data <- value$data_des
  if(is.null(data)) {
    return(NULL)
  }
  des_filter <- input$select_des_data_filter 
  if(des_filter == 'Year') {
    filter_year <- input$select_des_data_year
    if(filter_year == "") {
      return(NULL)
    }
    elements <- data.table(dbReadTable(concore, "elements"))
    return(
      data %>%
        left_join(elements %>% 
                    mutate(label = paste0(Name, " (", Unit, ")")) %>%
                    dplyr::select(ElementCode, label) %>%
                    mutate(ElementCode = as.character(ElementCode)),
                  by = 'ElementCode') %>%
        dplyr::select("FBS Code", "FBS Group", "label", as.character(filter_year)) %>%
        spread(
          value = as.character(filter_year),
          key = 'label'
        )
    )
  } else {
    filter_nutrient <- input$select_des_data_nutrient
    return(
      data %>%
        filter(Element == filter_nutrient) %>%
        dplyr::select(-c(Element, ElementCode, hidden))
    )
  }
})
output$download_total_des_level <- downloadHandler(
  filename = function() {
    "Nutrients_by_level.xlsx"
  },
  content = function(filename) {
    
    show_modal_spinner(
      
      spin = "cube-grid",
      color = "firebrick",
      text = "Preparing data, please wait...",
      session = shiny::getDefaultReactiveDomain()
    )
    
    wb <- get_download_total_des(by = 'level',input,session)
    saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    remove_modal_spinner()
  }
)
output$download_total_des_year <- downloadHandler(
  filename = function() {
    "Nutrients_SUA_by_year.xlsx"
  },
  content = function(filename) {
    show_modal_spinner(
      spin = "cube-grid",
      color = "firebrick",
      text = "Preparing data, please wait...",
      session = shiny::getDefaultReactiveDomain()
    )
    wb <- get_download_total_des(by = 'year',input,session)
    saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    remove_modal_spinner()
  }
)
output$total_des <-
  renderDataTable({
    data = des_table_data()
    if (is.null(data)){
      validate(
        need(nrow(data)>0, "Please run the balancing plugin")
      )
    }
  dt <- datatable (data, rownames= FALSE,
                     class = 'cell-border stripe',
                     extensions = c("FixedColumns","FixedHeader"),
                     options = list(
                       pageLength = 25,
                       fixedHeader= FALSE,
                       scrollX = TRUE,
                       scrollY = "500px"
                     )) 
    nutrient_cols <- colnames(data)
    nutrient_cols <- nutrient_cols[!(nutrient_cols %in% c('FBS Code', 'FBS Group', 'ElementCode', 'Element', 'hidden'))]
    dt <- dt %>% 
    formatCurrency(columns = nutrient_cols, currency = "", digits = 2, interval = 3, mark = ",")
 })  
  

output$fbs_balanced <-
  renderDataTable({
      if (is.null(value$data_fbs_balanced)){
        validate(
          need(nrow(value$data_fbs_balanced)>0, "Please run the FBS standardization plugin")
        )
      }
   datatable (value$data_fbs_balanced, rownames= FALSE,class = 'cell-border stripe',
                 extensions = c("FixedColumns","FixedHeader", "Buttons"),
                 options = list(
                   pageLength = 25,
                   dom= 'Blfrtip', buttons = I('colvis'),
                   # fixedHeader= TRUE,
                   scrollX = TRUE,
                   scrollY = "500px" ,
                   # autoWidth = T,
                   fixedColumns = list(leftColumns = 4),
                   columnDefs = list(
                     list(visible = FALSE, targets = (ncol(value$data_fbs_balanced)-1)))
                 ))  %>%
        formatStyle(0:ncol(value$data_fbs_balanced), valueColumns = "hidden",
                    `border-bottom` = styleEqual(1, "solid 3px")) %>%
        formatStyle('ElementCode', target = "row", color = styleEqual(unique(value$data_fbs_balanced$ElementCode),
                       ifelse(unique(value$data_fbs_balanced$ElementCode)%in% c('5166','664','665','674','684',
                                                      '261','271','281'),'blue','black')))%>%
        formatCurrency(columns = as.character(c(input$fromyear:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
})
  
}
