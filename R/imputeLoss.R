imputeLoss=function(input,output,session){
  show_modal_spinner(
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
)
 t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
 data_Session <- data.table(value$data_sua_unbalanced )
 data_Session <- long_format(data_Session)
if (unique(value_database$data$CountryM49) %in% "835"){
    itemLoss <- fread_rds("Data/lossRatio.rds")
    itemLoss=unique(itemLoss[,c("CPCCode")])
    itemLoss_session=data_Session[ElementCode == "5016"]
    itemLoss_session <- unique(itemLoss_session[,c("CPCCode")])
    itemLoss <- rbind(itemLoss,itemLoss_session)[!duplicated(itemLoss)]
    itemLoss <-itemLoss
    
  }else {
    itemLoss_session=data_Session[ElementCode == "5016"]
    itemLoss_session <- unique(itemLoss_session[,c("CPCCode")])
    itemLoss <- fread_rds("Data/lossRatio.rds")
    itemLoss <- unique(itemLoss[,c("CPCCode")])
    itemLoss <- rbind(itemLoss,itemLoss_session)
    itemLoss <-itemLoss[!duplicated(itemLoss)]
 }
  
lossData <- data_Session[
  ElementCode == 5016 & CPCCode %in% itemLoss$CPCCode
][
  is.na(Value), Flag := NA
][
  , c("ElementCode", "Element") := NULL
][
  , `:=`(
    `[5016] Loss [t]` = Value,
    `[5016] Flag` = Flag
  )
][
  , c("Value", "Flag") := NULL
]
lossData[, `[5016] Flag` := fifelse(
  !is.na(`[5016] Loss [t]`) & is.na(`[5016] Flag`), 
  "", 
  `[5016] Flag`
)]
  

# pull production Data
  productionData <- data_Session[ElementCode == "5510"][, c("ElementCode","Element") := NULL] 
  productionData <- productionData[ CPCCode %in% itemLoss$CPCCode]
  setnames(productionData, c("Value", "Flag"), c("[5510] Production [t]", "[5510] Flag"))
  productionData[, `[5510] Flag` := fifelse(
    !is.na(`[5510] Production [t]`) & is.na(`[5510] Flag`), 
    "", 
    `[5510] Flag`
  )]
  productionData[, `:=`(
    Year = as.character(Year),
    `[5510] Production [t]` = as.numeric(`[5510] Production [t]`)
  )]
#pull loss  ratio
ratios <-  fread_rds("Data/lossRatio.rds")
ratios <- long_format(ratios)
ratios$Flag =ifelse(!is.na(ratios$Value) & is.na(ratios$Flag), "", ratios$Flag)
setnames(ratios,"Flag","[Ratio] Flag")
ratios[, c("ElementCode", "Element") := NULL]
setnames(ratios,c("Value"),c("Ratios [%]"))
#Import data
importData <- data_Session[ElementCode == "5610"]
importData[, c("CountryM49","Country","ElementCode","Element") := NULL] 
importData <- importData[CPCCode %in% itemLoss$CPCCode]
setnames(importData, c("Value", "Flag"), c("[5610] Import Quantity [t]", "[5610] Flag"))
importData$`[5610] Flag` = ifelse(!is.na(importData$`[5610] Import Quantity [t]`) & is.na(importData$`[5610] Flag`), "", importData$`[5610] Flag`)

lossData[, `:=`(
  Year = as.character(Year),
  `[5016] Loss [t]` = as.numeric(`[5016] Loss [t]`)
)]
importData[, `:=`(
  `[5610] Import Quantity [t]` = as.numeric(`[5610] Import Quantity [t]`),
  Year = as.character(Year)
)]

data = merge(lossData[,-c("Commodity")],productionData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all  = TRUE)
data = merge(data,importData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all = TRUE)
data = merge(data,ratios[,-c("Commodity")],
               by = c("CPCCode","Year"),
               all= TRUE)
data[, Protected := ifelse(!is.na(`[5016] Loss [t]`) & `[5016] Flag` %in% c("","T","E"), TRUE, FALSE)] 
data <- merge(data,all_cpc, by = "CPCCode" , all.x = TRUE)

 for (j in t){
   officialData = filter(data, Year %in% c(2010:(j-1)))
   officialData$`Ratios [%]` = ifelse(is.na(officialData$`[Ratio] Flag`), 
        (officialData$'[5016] Loss [t]'/sum(officialData$`[5510] Production [t]`,officialData$`[5610] Import Quantity [t]`,na.rm = TRUE)
                                      )*100, officialData$`Ratios [%]`)
   officialData$`[Ratio] Flag` = ifelse(is.na(officialData$`[Ratio] Flag`) & !is.na(officialData$`Ratios [%]`), "I",officialData$`[Ratio] Flag`)
   calcRatios = aggregate(`Ratios [%]` ~ CPCCode,data = officialData,mean)
   calcRatios$`Ratios [%]` = round(calcRatios$`Ratios [%]`,0)
   calcRatios$`Ratios [%]` = ifelse(calcRatios$`Ratios [%]` < 0, 0 , calcRatios$`Ratios [%]`)
   setnames(calcRatios,"Ratios [%]","Ratios ReCalculated")
   imputedData = filter(data, Year %in% c(j) 
  )
   
imputedData = merge(imputedData,calcRatios,
                       by = "CPCCode",
                       all.x = TRUE)
imputedData$`Ratios [%]` = ifelse(is.na(imputedData$`Ratios [%]`) 
                         ,imputedData$`Ratios ReCalculated`,imputedData$`Ratios [%]`)
imputedData$`[Ratio] Flag` = ifelse(!imputedData$`[Ratio] Flag` %in% c("","T","E") & !is.na(imputedData$`Ratios [%]`), "I", imputedData$`[Ratio] Flag`)
imputedData$`Ratios ReCalculated` = NULL
setDT(imputedData)
imputedData[, Protected := ifelse( !`[5016] Flag` %in% c("","T","E") , FALSE, TRUE)]
imputedData[,new_loss := (`Ratios [%]`* sum(`[5510] Production [t]`,
                                               `[5610] Import Quantity [t]`,na.rm = TRUE)) /100, by= 1: nrow(imputedData)]
imputedData[Protected == FALSE, `[5016] Loss [t]` := new_loss]
imputedData[Protected == FALSE & !is.na(`[5016] Loss [t]`), `[5016] Flag` := "I"][, c("Protected","new_loss") :=NULL]
imputedData[, `[5016] Loss [t]` := round(`[5016] Loss [t]`,0)]
imputedData <- imputedData[!is.na(`[5016] Loss [t]`)]
imputedData <-imputedData[,c("CPCCode", "Year", "[5016] Loss [t]", "[5016] Flag")]
   
data <- merge(data, imputedData, by = c("CPCCode","Year"), all.x = TRUE)
data[, `[5016] Loss [t].x`:= ifelse(Protected == F & Year == j,`[5016] Loss [t].y`,`[5016] Loss [t].x`)]
data[, `[5016] Flag.x`:= ifelse(Protected == F & Year == j,`[5016] Flag.y`,`[5016] Flag.x`)][,c("[5016] Loss [t].y","[5016] Flag.y"):= NULL]
setnames(data,c("[5016] Loss [t].x","[5016] Flag.x"),c("[5016] Loss [t]","[5016] Flag"))
} 
finalLossData <- copy(data)
cols_to_convert <- c("Ratios [%]", "[5016] Loss [t]", "[5510] Production [t]")
finalLossData[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
finalLossData[, Protected := NULL]
lossValueData = melt(finalLossData[,-c("[5016] Flag","[5510] Flag","[5610] Flag","[Ratio] Flag")], 
                       id.vars = c("CPCCode","Commodity","Year"),
                       variable.name = "Element", value.name = "Value")
lossValueData$Element = ifelse(lossValueData$Element == "[5016] Loss [t]","Loss [t]",
                                 ifelse(lossValueData$Element == "[5510] Production [t]","Production [t]",
                                        # ifelse(lossValueData$Element == "[5071] Stock Variation [t]","Stock Variation [t]",
                                               ifelse(lossValueData$Element == "[5610] Import Quantity [t]","Import Quantity [t]",     
                                                      "Ratios [%]")))
lossFlagData <- data.table(melt(finalLossData[,-c("[5016] Loss [t]","[5510] Production [t]","[5610] Import Quantity [t]"
                                        ,"Ratios [%]")], 
                      id.vars = c("CPCCode","Commodity","Year"),
                      variable.name = "Flag Description", value.name = "Flag"))
lossFlagData$`Flag Description` = NULL
finalLossData <-  data.table(cbind(lossValueData,lossFlagData[,"Flag"]))
finalLossData$ElementCode <- ifelse(lossValueData$Element == "Loss [t]","5016",
                                     ifelse(lossValueData$Element == "Production [t]","5510",
                                            # ifelse(lossValueData$Element == "Stock Variation [t]","5071",
                                                   ifelse(lossValueData$Element == "Import Quantity [t]","5610",
                                                          ifelse(lossValueData$Element == "Export Quantity [t]","5910",
                                                                 "R5016"
                                                          ))))
  
finalLossData <- setcolorder(finalLossData, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
finalLossData <- finalLossData[order(CPCCode,Year)]
finalLossData[, Value := ifelse(ElementCode =="5016" & Value< 0 , NA, Value)]
finalLossData <-  finalLossData[ElementCode == "5016" & is.na(Value), Flag := NA][!duplicated(finalLossData[,c("CPCCode","Element","Year")])]
finalLossData[, Value := ifelse(Value == Inf, NA, Value)]
finalLossData[, Flag := ifelse(is.na(Value), NA, Flag)]
  
Sys.sleep(3)
remove_modal_spinner()
observeEvent(input$loss_imputation,{
    sendSweetAlert(
      session = session,
      title = "Imputed !!",
      text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
      type = "success"
    )
    
  })
return(finalLossData)
  
  
}