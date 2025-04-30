
fbs_report <- function(input,output,session){
observeEvent(input$fbs_report_button,{ 
t <- as.character(input$fbs_report_yr)
if (t != ""){
data <- fread_rds("SUA-FBS Balancing/FBS_Balanced/Data/fbs_balanced_final.rds")
#####fishery calories must be binned here      ##new
fish <- fread_rds("Data/fish.rds")
setnames(fish,c("Item Code (CPC)","Element Code","Year","Flag"),c("measuredItemFbsSua", "measuredElementSuaFbs","timePointYears", "flagObservationStatus"))
fish[,geographicAreaM49 := unique(data$geographicAreaM49)]  
data <- rbind(data,fish)
data <- data[timePointYears %in% t]
data[, measuredElementSuaFbs := as.character(measuredElementSuaFbs)]
# create empty columns for missing elements
if (!is.null(setdiff(c("5165","5520","5525","5016","5910","5610","5071"),unique(data$measuredElementSuaFbs)))){#often missing elements for a new year
missing_ele <-setdiff(c("5165","5520","5525","5016","5910","5610","5071"),unique(data$measuredElementSuaFbs))
missingBind <- as.data.table(expand.grid(measuredElementSuaFbs = as.character(missing_ele), timePointYears= t, 
           geographicAreaM49 = unique(data$geographicAreaM49), measuredItemFbsSua = "S2511", Value = NA_real_ ,
           flagObservationStatus= NA_character_))
data <- rbind(data,missingBind)
} 
# change nutritative factirs 
replacements = data.table(
  ele1  = unique(nutrientEle$ElementCode), 
  ele2 = unique(nutrientEle$measuredElement)
)
data <- merge(data,replacements, by.x = "measuredElementSuaFbs", by.y = "ele2", all.x = TRUE)
data[, measuredElementSuaFbs := ifelse(!is.na(ele1),  ele1, measuredElementSuaFbs)]
data[, ele1 := NULL]
##2901 grand Total (Including Fishery)
##2902 grand total (excluding Fishery)
tempdata <- data[measuredItemFbsSua %in% c("S2901", "S2960") & measuredElementSuaFbs %in% c("664","674","684")] [,c("measuredItemFbsSua","measuredElementSuaFbs","Value"), with = F] 
tempdata[, new := sum(Value[measuredItemFbsSua %in% "S2901"], Value[measuredItemFbsSua %in% "S2960"],na.rm = TRUE),.(measuredElementSuaFbs)]
tempdata <- unique(tempdata[,c("measuredElementSuaFbs","new"),with =F])
tempdata[, measuredItemFbsSua := "S2901"]
setnames(tempdata,"new","Value")
data[, c("timePointYears","geographicAreaM49", "flagObservationStatus") := NULL]
#####new here we have to bind 2901 and change the exisitng code 2901 to 2902     #new
data[measuredItemFbsSua == "S2901" & measuredElementSuaFbs %in% c("664","674","684"),  ':=' (measuredItemFbsSua ="S2902")]
data <- rbind(data,tempdata)
#total nutrient at total level
###add new nutrient at total level
NutriTotData <- copy(data)[measuredItemFbsSua %in% c("S2903","S2941") & measuredElementSuaFbs %in% new_nutrient_element]
NutriTotData <- aggregate(
  Value ~ measuredElementSuaFbs, NutriTotData,
  sum, na.rm = TRUE)
NutriTotData <- data.table(NutriTotData)
NutriTotData[, measuredItemFbsSua := "S2902"]
data <- rbind(data,NutriTotData)
data[,measuredElementSuaFbs := as.character(measuredElementSuaFbs)]
data <- merge(data, all_elements, by.x = "measuredElementSuaFbs",by.y = "ElementCode",all.x = TRUE)
#removing 665, 4031, 4030 , 4008, 4023, 4014 elements as suggested by Rachele 28/04/2023 remove 4001 -edible
data <- data[!measuredElementSuaFbs %in% c("665","4001", "4031", "4030" , "4008", "4023", "4014")]
data[measuredElementSuaFbs == 5510, Element := "Prod."]
data[measuredElementSuaFbs == 5610, Element := "Imp."]
data[measuredElementSuaFbs == 5071, Element := "Stock Var."]
data[measuredElementSuaFbs == 5910, Element := "Exp."]
data[measuredElementSuaFbs == 5141, Element := "Food"]
data[measuredElementSuaFbs == 5023, Element := "Proc."]
data[measuredElementSuaFbs == 5520, Element := "Feed"]
data[measuredElementSuaFbs == 5525, Element := "Seed"]
data[measuredElementSuaFbs == 5016, Element := "Losses"]
data[measuredElementSuaFbs == 5165, Element := "Indus."]
data[measuredElementSuaFbs == 5166, Element := "Resid."]
data[measuredElementSuaFbs == 665, Element := "Kg/Yr"] ########## conversion is needed "Grams(/capita/day)[g]" removed
data[measuredElementSuaFbs == 664, Element := "KCal/Day"]
data[measuredElementSuaFbs == 674, Element := "Protein g/Day"]
data[measuredElementSuaFbs == 684, Element := "Fat g/Day"]
data[measuredElementSuaFbs == 4001, Element := "Edible Q.[g]"] #removed
data[measuredElementSuaFbs == 4006, Element := "Carbo.[g]"]
data[measuredElementSuaFbs == 4007, Element := "Fibre.[g]"]
data[measuredElementSuaFbs == 4008, Element := "Alcohol [g]"] #removed
data[measuredElementSuaFbs == 4009, Element := "Cal.[mg]"]
data[measuredElementSuaFbs == 4010, Element := "Iron [mg]"]
data[measuredElementSuaFbs == 4011, Element := "Mag.[mg]"]
data[measuredElementSuaFbs == 4012, Element := "Phosp.[mg]"]
data[measuredElementSuaFbs == 4013, Element := "Pota.[mg]"]
data[measuredElementSuaFbs == 4014, Element := "Sod.[mg]"] #removed
data[measuredElementSuaFbs == 4015, Element := "Zinc[mg]"]
data[measuredElementSuaFbs == 4017, Element := "Vit.A[mcg RE]"]
data[measuredElementSuaFbs == 4018, Element := "Vit.A[mcg RAE]"]
data[measuredElementSuaFbs == 4021, Element := "Thia.[mg]"]
data[measuredElementSuaFbs == 4022, Element := "Rib.[mg]"]
data[measuredElementSuaFbs == 4023, Element := "Nian.[mg]"] #removed
data[measuredElementSuaFbs == 4029, Element := "Vit.C [mg]"]
data[measuredElementSuaFbs == 4030, Element := "Water [g]"] #removed
data[measuredElementSuaFbs == 4031, Element := "Ash [g]"]#removed
#add the population
popData <- fread_rds("SUA-FBS Balancing/Data/popSWS.rds")
popData <- popData[timePointYears %in% t]
data[,measuredElementSuaFbs := NULL]
### add popultaion as a row 
population_data <- data.table(measuredItemFbsSua = c("511") , Value = round(as.numeric(popData$Value),0), Element = c("Population") )
# data <- rbind(population_data,data)
data <- dcast.data.table(data, measuredItemFbsSua ~ Element, value.var = c("Value"))
data[, c("Calories/Year [kcal]","Proteins/Year [g]","Fats/Year [g]") := NULL]
#convert 665 to kg and year
# data[, `Kg/Yr` := (`Kg/Yr`/1000)*365] removed requested by Rachele
data[is.na(Prod.), Prod. := 0]
data[is.na(Imp.), Imp. := 0]
data[is.na(`Stock Var.`), `Stock Var.` := 0]
data[, Total := Prod.+Imp. - `Stock Var.` ]
#merging fbs names
commodityName <- fread_rds("Data/SUA_Commodities.rds")
data <-merge(data,commodityName,by.x = "measuredItemFbsSua",by.y = "CPCCode",all.x = TRUE)
setnames(data, "Commodity","Item")
#### new - change name of 2901 as Grand Total(Incl. Fish, Seafood)   #new
data[measuredItemFbsSua == "S2901", Item := "Grand Total (incl. Fish, Seafood)"] #new
#delete 2902 grand total excluding fish
data[measuredItemFbsSua == "S2902", Item := "Grand Total (excl. Fish, Seafood)"]
grandTotal <- data[measuredItemFbsSua %in% c("S2901","S2902")]
data <- data[!measuredItemFbsSua %in% c("S2901","S2902")]
data <- data[order(measuredItemFbsSua)]
data <- rbind(grandTotal,data)
data[, measuredItemFbsSua := NULL]
if (!"Proc." %in% names(data)){
  data[, `Proc.` := NA_real_]
}
data_order <- c("Item", "Prod.", "Imp.", "Stock Var.","Exp.", "Total", "Food","Proc.","Feed", "Seed", "Losses", "Indus.", "Resid.",
                "KCal/Day","Protein g/Day","Fat g/Day")
# data_order <- c("Item", "Prod.", "Imp.", "Stock Var.","Exp.", "Total", "Food","Proc.","Feed", "Seed", "Losses", "Indus.", "Resid.",
#                 "Kg/Yr", "KCal/Day","Protein g/Day","Fat g/Day")
setcolorder(data,c(data_order,names(data)[!names(data) %in% data_order]))
# data[-1, Pop. := NA]
data[1, c("Prod.","Imp.", "Stock Var.", "Total") := NA]
#Divied quantities from 1000 MT
column_metric <- c("Prod.", "Imp.", "Stock Var.","Exp.", "Total", "Food","Proc.","Feed", "Seed", "Losses", "Indus.", "Resid.")
# columns_to_round <- c("Prod.", "Imp.", "Stock Var.","Exp.", "Total", "Food","Proc.","Feed", "Seed", "Losses", "Indus.", "Resid.",
#                       "Kg/Yr","KCal/Day","Protein g/Day","Fat g/Day","Alcohol [g]","Ash [g]","Cal.[mg]","Carbo.[g]","Edible Q.[g]" ,"Fibre.[g]" ,
#                       "Iron [mg]","Mag.[mg]","Nian.[mg]" ,"Phosp.[mg]","Pota.[mg]" ,"Rib.[mg]", "Sod.[mg]" , "Thia.[mg]" ,
#                       "Vit.A[mcg RAE]" ,"Vit.A[mcg RE]","Vit.C [mg]", "Water [g]", "Zinc[mg]")
columns_to_round <- c("Prod.", "Imp.", "Stock Var.","Exp.", "Total", "Food","Proc.","Feed", "Seed", "Losses", "Indus.", "Resid."
                      ,"KCal/Day","Protein g/Day","Fat g/Day","Cal.[mg]","Carbo.[g]" ,"Fibre.[g]" ,
                      "Iron [mg]","Mag.[mg]" ,"Phosp.[mg]","Pota.[mg]" ,"Rib.[mg]" , "Thia.[mg]" ,
                      "Vit.A[mcg RAE]" ,"Vit.A[mcg RE]","Vit.C [mg]", "Zinc[mg]")
data[ , (column_metric) := lapply(.SD, "/", 1000), .SDcols = column_metric]
data[,(columns_to_round) := round(.SD,0), .SDcols = columns_to_round]
#removing zero
data[2, c("Prod.","Imp.", "Stock Var.", "Total") := NA]
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    # tr(
    #   th(rowspan = 18, 'Food Balance Sheet 2018')
    #   ),
    tr(
      th(rowspan = 3, 'Item'),
      # th(rowspan = 2, 'Pop.'),
      th(colspan = 5, 'Domestic Supply'),
      th(colspan = 9, 'Domestic Utilization'),
      th(colspan = 16, 'Per capita supply')
    ),
    tr(
    
      lapply(rep(c('Prod.', 'Imp.','Stock Var.', 'Exp.', 'Tot.','Food', 'Proc.', 'Feed','Seed','Loss.','Indu.','Resid.'), 1), th),
      th('Energy kcal/Day/cap'),
      # th('Total'),
      lapply(rep(c('Prot. g/Day/Cap', 'Fat g/Day/Cap','Cal.[mg]','Carbo.[g]' ,'Fibre.[g]' ,'Iron [mg]','Mag.[mg]'
                    ,'Phosp.[mg]','Pota.[mg]' ,'Rib.[mg]' , 'Thia.[mg]' ,'Vit.A[mcg RAE]' ,'Vit.A[mcg RE]','Vit.C [mg]', 'Zinc[mg]'), 1), th)
    ),
   tr(
      # lapply(rep(c('1000'), 1), th),
      th(colspan = 14, '(1000 Tonnes)'),
      th(colspan = 21, 'g/Day/Cap')

    )
  )
))
#new with excel download 
data <- DT::datatable(data,container = sketch,rownames = FALSE, caption = 
      paste("* Supply and Utilization components are in 1,000 MT. Population is", 
      format( round(as.numeric(popData$Value),0),nsmall=0, big.mark=","), "(1,000)"), 
      extensions = c('Buttons',"FixedColumns","FixedHeader"), 
      options = list(dom = 'Blfrtip',
      orientation ='portrait',
      buttons =  list( list(extend = 'csv', filename = 'FBS Report',title = paste(value_database$data$Country, "-" ,t,  "Food Balance Sheet")), 
        list(extend = 'excel', filename = 'FBS Report',title = paste(value_database$data$Country, "-" ,t,  "Food Balance Sheet")), 
        list(extend = 'pdf', pageSize = 'A3',orientation = 'landscape',filename = 'FBS Report',title = paste(value_database$data$Country, "-" ,t,  "Food Balance Sheet")), 
        list(extend = 'print', filename = 'FBS Report',title = paste(value_database$data$Country, "-" ,t,  "Food Balance Sheet"))),
        scrollX=TRUE,fixedHeader= FALSE,scrollY = "300px")) %>%
  formatCurrency(columns =2:ncol(data),currency = "", digits = 0,interval = 3, mark = ",")
              value$data_fbs_report <- data
#browser()
}else if (t == ""){
  validate(
    need(nrow(data)>0, "Please select an year in the FBS tab to get the FBS Report")
  )
}
return(data)
})

output$download_fbs_report<- downloadHandler(
  filename = function() {
    "table.xlsx"
  },
  content = function(file) {
    data_download <- fbs_report()
    browser()
    write.xlsx(data_download ,file,row.names = FALSE)
  }
)




output$fbs_report <-
renderDT(server=FALSE,value$data_fbs_report)
}
  
  


