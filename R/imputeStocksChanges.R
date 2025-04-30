imputeStocksChanges= function(input,output,session){
show_modal_spinner(
spin = "cube-grid",
color = "firebrick",
text = "Please wait...",
session = shiny::getDefaultReactiveDomain()
)
t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
data_Session <- data.table(long_format(data.table(value$data_sua_unbalanced )))
data_Session <- data_Session[,Year := as.character(Year)]
stockVarData <- data_Session[ElementCode %in% c("5071") & Year %in% c(2010: input$endyear)]
openingStock <- data_Session[ElementCode %in% c("5113")]
stockable_items <- readRDS("SUA-FBS Balancing/Data/utilization_table_2018.rds")[stock == 'X', cpc_code]
stockVarData[, c("CountryM49","Country","ElementCode","Element", "Commodity") := NULL] 
stock_Data_bind <- stockVarData[Year %in% as.numeric(2010: (t[1]-1))]
setnames(stockVarData,old = c("Value", "Flag"),new = c("[5071] Stock Variation [t]", "[5071] Flag"))
stockVarData$`[5071] Stock Variation [t]` = as.numeric(stockVarData$`[5071] Stock Variation [t]`)
#stockable 
stockVarData$stockable = ifelse(stockVarData$CPCCode %in% stockable_items , TRUE, FALSE)
#create a grid for stockable items
stock_item_data_frame <- data.table(expand.grid(CPCCode = stockable_items, Year = as.character(c(2010:input$endyear)), stockable = TRUE ))
stock_item_data_frame[,Year := as.character(Year)]
stockVarData[,Year := as.character(Year)]
stock_item_data_frame <- merge(stock_item_data_frame,stockVarData, by=c("CPCCode","Year"),all.x = T)
stock_item_data_frame[, stockable.x := ifelse(is.na(stockable.x)  , FALSE, stockable.x)]
stock_item_data_frame[, stockable.y := NULL ]
setnames(stock_item_data_frame, "stockable.x", "stockable" )
#assign flag NA for NA values. 
stock_item_data_frame$`[5071] Flag` =ifelse(is.na(stock_item_data_frame$`[5071] Stock Variation [t]`) & stock_item_data_frame$`[5071] Flag` == "", NA, 
                                            stock_item_data_frame$`[5071] Flag`)
#assign flag "" for offcial data where it appears as NA.
stock_item_data_frame$`[5071] Flag` =ifelse(!is.na(stock_item_data_frame$`[5071] Stock Variation [t]`) & 
                                              is.na(stock_item_data_frame$`[5071] Flag`), "", stock_item_data_frame$`[5071] Flag`)
#Assign if a value is protected 
stock_item_data_frame$Protected = ifelse((stock_item_data_frame$Year %in% c(2010:(t[1]-1)) &
                                           !is.na(stock_item_data_frame$`[5071] Stock Variation [t]`))| stock_item_data_frame$'[5071] Flag' %in% c("","T","E"),
                                         TRUE, FALSE)
# pull production
productionData=data_Session[ElementCode == "5510" & Year %in% c(2010:input$endyear)]
productionData[, c("CountryM49","Country","ElementCode","Element", "Commodity") := NULL] 
setnames(productionData,old = c("Value", "Flag"),new = c("[5510] Production [t]", "[5510] Flag"))  
productionData$`[5510] Flag` =ifelse(!is.na(productionData$`[5510] Production [t]`) & is.na(productionData$`[5510] Flag`), "", productionData$`[5510] Flag`)
  
#pull trade data
tradeData<- data_Session[ElementCode %in% c("5610", "5910") & Year %in% c(2010:input$endyear) ]
#filter import data  
importData = tradeData[ElementCode == 5610]
importData[, c("CountryM49","Country","ElementCode","Element", "Commodity") := NULL]
setnames(importData,old = c("Value", "Flag"),new = c("[5610] Import Quantity [t]", "[5610] Flag"))  
importData$`[5610] Flag` =ifelse(!is.na(importData$`[5610] Import Quantity [t]`) & is.na(importData$`[5610] Flag`), "", importData$`[5610] Flag`)
#filter export data
exportData <- tradeData[ElementCode == 5910 ]
exportData[, c("CountryM49","Country","ElementCode","Element", "Commodity") := NULL]
setnames(exportData,old = c("Value", "Flag"),new = c("[5910] Export Quantity [t]", "[5910] Flag"))  
exportData$`[5910] Flag` =ifelse(!is.na(exportData$`[5910] Export Quantity [t]`) & is.na(exportData$`[5910] Flag`), "", exportData$`[5910] Flag`)
  
# merge production , import and export data
data_list <- list(stock_item_data_frame, productionData, importData, exportData)

# Apply transformation to each data table
data_list <- lapply(data_list, function(dt) {
  as.data.table(dt)[, Year := as.character(Year)]
})
# Assign back to the original data tables
stock_item_data_frame <- data_list[[1]]
productionData <- data_list[[2]]
importData <- data_list[[3]]
exportData <- data_list[[4]]
data <- Reduce(function(x, y) merge(x, y, by = c("CPCCode", "Year"), all.x = TRUE), data_list)
Availability_t = c()
cols_to_convert <- c("[5510] Production [t]", "[5610] Import Quantity [t]", "[5910] Export Quantity [t]")
data[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
for(i in 1:dim(data)[1]){
  Availability_t[i] = sum(data[i,]$'[5510] Production [t]',
                          data[i,]$'[5610] Import Quantity [t]',
                          - data[i,]$'[5910] Export Quantity [t]',na.rm = TRUE)
}
data$Availability_t = Availability_t
data$`[Availability_t] Flag` = "I"
data[, AvailabilityVar := Availability_t - shift(Availability_t, type = "lag"),by = "CPCCode"]
data$`[AvailabilityVar] Flag` = "I"
subData <- lapply(split(data, by = "CPCCode", drop = TRUE), as.data.table)
splineFit = list()
rmCodes = c()
for(i in 1:length(subData)){
 if(all(is.na(subData[[i]]$`[5071] Stock Variation [t]`)) ||  all(is.na(subData[[i]]$AvailabilityVar))) {
   rmCodes[i] = unique(subData[[i]]$CPCCode)
  } else{
    splineFit[[i]] = splinefun(subData[[i]]$AvailabilityVar,subData[[i]]$`[5071] Stock Variation [t]`,
                                 method = "fmm", ties = mean)
    subData[[i]][, new_stock := splineFit[[i]](c(AvailabilityVar))]
    subData[[i]][, new_stock := ifelse(Availability_t < 0 , 0,new_stock )]
    subData[[i]][, new_stock := ifelse(Availability_t > 0 & new_stock > Availability_t, Availability_t,new_stock )]
    subData[[i]][, new_stock_flag:= "I"]
}
 }
rmCodes = rmCodes[!is.na(rmCodes)]
okCodes = setdiff(names(subData),rmCodes)
if (length(okCodes) == 0){
  finalData <- NULL
  }else{
    subData = subData[c(okCodes)]
    finalData = do.call("rbind",subData)
    finalData <- finalData[Year %in% t]
    finalData[Protected == FALSE, `:=`(
      `[5071] Stock Variation [t]` = new_stock,
      `[5071] Flag` = new_stock_flag
    )]
    # Keep only needed columns first (so we're working with less data early)
    finalData <- finalData[, .(
      CPCCode, Year, stockable,
      `[5071] Stock Variation [t]`,
      `[5071] Flag`,
      Protected, new_stock, new_stock_flag
    )]
    # Filter out unwanted rows
    finalData <- finalData[!(Protected == FALSE & stockable == FALSE)]
    # Drop temporary columns
    finalData[, c("new_stock", "new_stock_flag", "stockable", "Protected") := NULL]
    # Rename columns
    setnames(finalData,
             old = c("[5071] Stock Variation [t]", "[5071] Flag"),
             new = c("Value", "Flag"))
   #take protected data for year t from rmcodes. Even it is not stockble, take it anyway. Priority is given to protected data.
   protected_data_t <- data[CPCCode %in% rmCodes & Year %in% t & Protected == TRUE]
   protected_data_t <- protected_data_t[,c("CPCCode","Year","[5071] Stock Variation [t]","[5071] Flag")]
   setnames(protected_data_t,c("[5071] Stock Variation [t]","[5071] Flag"),c("Value","Flag"))
   stock_Data_bind[, stockable := NULL]
   # setnames(stock_Data_bind, c("[5071] Stock Variation [t]","[5071] Flag"),c("Value","Flag"))
   finalData <-rbind(finalData,protected_data_t,stock_Data_bind)
   finalData[, `:=` (ElementCode = "5071",
                      Element = "Stock Variation [t]") ]
   finalData <- merge(finalData,all_cpc, by="CPCCode",all.x = T)
   openingStock[, c("CountryM49","Country") := NULL]
   finalData <-  rbind(finalData,openingStock)
   setcolorder(finalData, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
   x <- copy(finalData[Year %in% c(2014 : input$endyear)])
   x[,c("Commodity","Element") := NULL]
   x <- dcast.data.table(x, CPCCode+ Year ~ ElementCode, value.var = c("Value","Flag"))
   setnames(x, c("Value_5071","Value_5113"),c("delta","new_opening"))
   x[is.na(delta), delta := 0]
   x[is.na(new_opening), new_opening := 0]
   x <- x[order(CPCCode, Year)]
   groups <- unique(x[, .(CPCCode)])
   res <- list()
   for (i in seq_len(nrow(groups))) {
      z <- x[groups[i], on = c( "CPCCode")]
      if (nrow(z) > 1) {
        for (j in seq_len(nrow(z))[-1]) {
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
data_2013 <- finalData[Year %in% c(2010:2013)]
updated_stock <- rbindlist(res)
ValueData <- data.table (melt(updated_stock[,-c("Flag_5071","Flag_5113")], 
                          id.vars = c("CPCCode","Year"),
                          variable.name = "ElementCode", value.name = "Value"))
ValueData[, ElementCode := fcase(
  ElementCode == "delta", "5071",
  ElementCode == "new_opening", "5113",
  default = ElementCode
)]
FlagData = data.table(melt(updated_stock[,-c("delta","new_opening")], 
                          id.vars = c("CPCCode","Year"),
                          variable.name = "Flag_Des", value.name = "Flag"))
FlagData[, Flag_Des := NULL]
update_Stock_final_data <- cbind(ValueData, FlagData[,"Flag"])
update_Stock_final_data <- merge(update_Stock_final_data,all_cpc, by="CPCCode",all.x = T)
update_Stock_final_data <- merge(update_Stock_final_data,all_elements, by="ElementCode",all.x = T)
finalData <- rbind(data_2013,update_Stock_final_data)
finalData = finalData[order(CPCCode,Year)]
Sys.sleep(5)
remove_modal_spinner()
observeEvent(input$stock_imputation,{
      sendSweetAlert(
        session = session,
        title = "Imputed !!",
        text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
        type = "success"
      )
})
}
return(finalData)
}

