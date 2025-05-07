imputeFeed=function(input,output,session){

#Feed Module only imputes values for NA in the year t. Official data are c(2010:t) or Flag %in% c("", T). If you have a figure with flag I,
#in t, then it will not be touched. Only touched if NA in year t. 
  show_modal_spinner(
   spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
  )
t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
data_Session <- data.table(value$data_sua_unbalanced )
data_Session <- long_format(data_Session)
write.csv(data_Session,"data_Session.csv",row.names = F)
if (unique(value_database$data$CountryM49) %in% "835"){
    itemFeed <- data.table(dbReadTable(con,"feed_ratios"))
    itemFeed=unique(itemFeed[,c("CPCCode", "Commodity")])
    itemFeed_session=subset(data_Session, ElementCode == "5520")
    itemFeed_session <- unique(itemFeed_session[,c("CPCCode","Commodity")])
    itemFeed <- rbind(itemFeed,itemFeed_session)
    itemFeed <-itemFeed[!duplicated(itemFeed[,"CPCCode"])]
    
  }else {
    itemFeed_session=subset(data_Session, ElementCode == "5520")
    itemFeed_session <- unique(itemFeed_session[,c("CPCCode","Commodity")])
    itemFeed <- data.table(dbReadTable(con,"feed_ratios"))
    itemFeed=unique(itemFeed[,c("CPCCode", "Commodity")])
    itemFeed <- rbind(itemFeed,itemFeed_session)
    itemFeed <-itemFeed[!duplicated(itemFeed[,"CPCCode"])]
  }
# pull feed ratios
ratios <- data.table(dbReadTable(con,"feed_ratios"))[,c("StatusFlag","LastModified") := NULL]
ratios$Flag =ifelse(!is.na(ratios$Value) & is.na(ratios$Flag), "", ratios$Flag)
setnames(ratios,"Flag","[Ratio] Flag")
ratios[,c("ElementCode", "Element")] <- NULL  
setnames(ratios,c("Value"),c("Ratios [%]"))
#pull Trade Data
tradeData=subset(data_Session , ElementCode %in% c("5610", "5910") & Year %in% c(2010:t[length(t)]))
importData = tradeData[ElementCode == 5610 & CPCCode %in% itemFeed$CPCCode]
importData[, c("ElementCode","Element") := NULL]
setnames(importData, c("Value", "Flag"), c("[5610] Import Quantity [t]", "[5610] Flag"))  
importData$`[5610] Flag` =ifelse(!is.na(importData$`[5610] Import Quantity [t]`) & is.na(importData$`[5610] Flag`), "", importData$`[5610] Flag`)
  
exportData = tradeData[ElementCode == 5910 & CPCCode %in% itemFeed$CPCCode]
exportData[, c("ElementCode","Element") := NULL]
setnames(exportData, c("Value", "Flag"), c("[5910] Export Quantity [t]", "[5910] Flag"))
exportData$`[5910] Flag` =ifelse(!is.na(exportData$`[5910] Export Quantity [t]`) & is.na(exportData$`[5910] Flag`) , "", exportData$`[5910] Flag`)
# pull feed data
feedData = data_Session[ElementCode == 5520 & CPCCode %in% itemFeed$CPCCode]
feedData[is.na(Value), Flag := NA ]
feedData[, c("ElementCode","Element") := NULL]
setnames(feedData, c("Value", "Flag"), c("[5520] Feed [t]", "[5520] Flag"))
feedData$`[5520] Flag` =ifelse(!is.na(feedData$`[5520] Feed [t]`) & is.na(feedData$`[5520] Flag`), "", feedData$`[5520] Flag`)
  
# pull production data
productionData <- data_Session[ElementCode == "5510" & CPCCode %in% itemFeed$CPCCode ]
productionData[, c("ElementCode","Element") := NULL] 
setnames(productionData, c("Value", "Flag"), c("[5510] Production [t]", "[5510] Flag"))
productionData$`[5510] Flag` =ifelse(!is.na(productionData$`[5510] Production [t]`) & is.na(productionData$`[5510] Flag`), "", productionData$`[5510] Flag`)
########### Data Merging   
data = merge(feedData[,-c("Commodity")],productionData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all = TRUE)
data = merge(data,importData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all = TRUE)
data = merge(data,exportData[,-c("Commodity")],
               by = c( "CPCCode","Year"),
               all = TRUE)
data = merge(data,ratios[,-c("Commodity")],
               by = c("CPCCode","Year"),
               all.x  = TRUE)
data[, Protected := ifelse(!is.na(`[5520] Feed [t]`) & `[5520] Flag` %in% c("","T","E"), TRUE, FALSE)] 
ProdNT = c()
  for(i in 1:dim(data)[1]){
    ProdNT[i] = sum(data[i,]$'[5510] Production [t]',
                    data[i,]$'[5610] Import Quantity [t]', 
                    - data[i,]$'[5910] Export Quantity [t]',na.rm = TRUE)
  }
data$ProdNT = ProdNT
data$`[ProdNT] Flag` = "I"
data <- data.table(merge(data,all_cpc, by = "CPCCode" , all.x = TRUE))
for (j in t){
    officialData = filter(data, Year %in% c(2010:(j-1)))
   #changed here
    officialData$`Ratios [%]` = ifelse(is.na(officialData$`[Ratio] Flag`) & officialData$ProdNT != 0, 
                                       (officialData$'[5520] Feed [t]'/officialData$ProdNT)*100, officialData$`Ratios [%]`)
    officialData$`Ratios [%]` = round(officialData$`Ratios [%]`,0)
    officialData$`[Ratio] Flag` = ifelse(is.na(officialData$`[Ratio] Flag`) & !is.na(officialData$`Ratios [%]`), "I",officialData$`[Ratio] Flag`)
    calcRatios = aggregate(`Ratios [%]` ~ CPCCode,data = officialData,mean)
    calcRatios$`Ratios [%]` <- pmax(round(calcRatios$`Ratios [%]`, 0), 0)
    setnames(calcRatios,"Ratios [%]","Ratios ReCalculated")
    imputedData = filter(data, Year %in% c(j) )
    imputedData = data.table (merge(imputedData,calcRatios,
                        by = "CPCCode",
                        all.x = TRUE))
    imputedData$`Ratios [%]` = ifelse(is.na(imputedData$`Ratios [%]`) 
                                      ,imputedData$`Ratios ReCalculated`,imputedData$`Ratios [%]`)
    imputedData$`[Ratio] Flag` = ifelse(!imputedData$`[Ratio] Flag` %in% c("","T","E") & !is.na(imputedData$`Ratios [%]`), 
                                        "I", imputedData$`[Ratio] Flag`)
    imputedData$`Ratios ReCalculated` = NULL
    imputedData[, Protected := ifelse( !`[5520] Flag` %in% c("","T","E") , FALSE, TRUE)]
    imputedData[, new_feed :=`Ratios [%]`*ProdNT/100, by= 1: nrow(imputedData) ]
    imputedData[Protected == FALSE, `[5520] Feed [t]` := new_feed]
    imputedData[Protected == FALSE & !is.na(`[5520] Feed [t]`), `[5520] Flag` := "I"]
    imputedData[, c("Protected","new_feed") :=NULL]
    imputedData[,`[5520] Feed [t]` := round(`[5520] Feed [t]`,0)]
    imputedData=imputedData[!is.na(`[5520] Feed [t]`)]
    imputedData <- imputedData[,c("CPCCode", "Year", "[5520] Feed [t]", "[5520] Flag")]
    data <-data.table( merge(data, imputedData, by = c("CPCCode","Year"), all.x = TRUE))
    data[, `[5520] Feed [t].x`:= ifelse(Protected == F & Year == j,`[5520] Feed [t].y`,`[5520] Feed [t].x`)]
    data[, `[5520] Flag.x`:= ifelse(Protected == F & Year == j,`[5520] Flag.y`,`[5520] Flag.x`)]
    data[,c("[5520] Feed [t].y","[5520] Flag.y"):= NULL]
    setnames(data,c("[5520] Feed [t].x","[5520] Flag.x"),c("[5520] Feed [t]","[5520] Flag"))
  }
  data[,Year := as.character(Year)]
  finalFeedData <- copy(data)
  finalFeedData <- finalFeedData[!is.na(`[5520] Feed [t]`)]
  finalFeedData[, c("Ratios [%]", "[5610] Import Quantity [t]", "[5910] Export Quantity [t]")] <- 
    lapply(finalFeedData[, c("Ratios [%]", "[5610] Import Quantity [t]", "[5910] Export Quantity [t]")], as.numeric)
  finalFeedData[, Protected := NULL]
  
  feedValueData = melt(finalFeedData[,-c("[5610] Flag","[5910] Flag","[5510] Flag","[5520] Flag","[Ratio] Flag","[ProdNT] Flag")], 
                 id.vars = c("CPCCode","Commodity","Year"),
                       variable.name = "Element", value.name = "Value")
  feedValueData$Element = ifelse(feedValueData$Element == "[5520] Feed [t]","Feed [t]",
                                 ifelse(feedValueData$Element == "[5510] Production [t]","Production [t]",
                                        ifelse(feedValueData$Element == "[5610] Import Quantity [t]","Import Quantity [t]",
                                               ifelse(feedValueData$Element == "[5910] Export Quantity [t]","Export Quantity [t]",
                                                      ifelse(feedValueData$Element == "ProdNT","ProdNT [t]",
                                                             "Ratios [%]"
                                                                    )))))
  feedFlagData = data.table(melt(finalFeedData[,-c("[5610] Import Quantity [t]","[5910] Export Quantity [t]","[5510] Production [t]",
                                        "[5520] Feed [t]","Ratios [%]","ProdNT")],
                      id.vars = c("CPCCode","Commodity","Year"),
                      variable.name = "Flag Description", value.name = "Flag"))
  feedFlagData$`Flag Description` = NULL
  # finalseedData
  finalFeedData = cbind(feedValueData,feedFlagData[,"Flag"])
  finalFeedData$ElementCode = ifelse(feedValueData$Element == "Feed [t]","5520",
                                     ifelse(feedValueData$Element == "Production [t]","5510",
                                            ifelse(feedValueData$Element == "Import Quantity [t]","5610",
                                                   ifelse(feedValueData$Element == "Export Quantity [t]","5910",
                                                          ifelse(feedValueData$Element == "ProdNT","xxxx",
                                                                 "R5520"
                                                                        )))))
  finalFeedData = data.table(setcolorder(finalFeedData, c("CPCCode","Commodity","ElementCode",
                                                          "Element","Year","Value","Flag")))
  finalFeedData = finalFeedData[order(CPCCode,Year)]
  finalFeedData[, Value := ifelse(ElementCode =="5520" & Value< 0 , NA, Value)]
  finalFeedData <-  finalFeedData[ElementCode == "5520" & is.na(Value), Flag := NA]
  finalFeedData <- finalFeedData[!duplicated(finalFeedData[,c("CPCCode","Element","Year")])]
  finalFeedData[, Value := ifelse(Value == Inf, NA, Value)]
  finalFeedData[, Flag := ifelse(is.na(Value), NA, Flag)]
  finalFeedData
  Sys.sleep(3)
  remove_modal_spinner()
 #for some t values feed is not imputed because ratio is missing and also the prodNT is zero.  
  observeEvent(input$feed_imputation,{
    sendSweetAlert(
      session = session,
      title = "Imputed !!",
      text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
      type = "success"
    )
    
  })
return(finalFeedData)
  
  
}