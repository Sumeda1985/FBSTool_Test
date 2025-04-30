
update_stock_domain <-  function(input,output,session){
observe ({
  stockData <- data.table(dbReadTable(con, "dbcountry"))[StatusFlag == 1]
  stockData <- stockData[ElementCode %in% c( "5113","5071")]
  stockData[,c("StatusFlag","LastModified") := NULL]
  stockData <- stockData[!duplicated(stockData,by=c("CPCCode","Commodity","ElementCode","Element","Year"))]
  stockData[, c("CountryM49","Country"):=NULL]
  stockData <- stockData[Year %in% c(2010 : input$endyear)]
  #updating opening stocks 
  x <- copy(stockData[Year %in% c(2014 : input$endyear)])
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
  
  data_2013 <- stockData[Year %in% c(2010:2013)]
  updated_stock <- rbindlist(res)
  ValueData = data.table(melt(updated_stock[,-c("Flag_5071","Flag_5113")], 
                              id.vars = c("CPCCode","Year"),
                              variable.name = "ElementCode", value.name = "Value"))
  ValueData[ElementCode == "delta", ElementCode := "5071"]
  ValueData[ElementCode == "new_opening", ElementCode := "5113"]
  FlagData = data.table(melt(updated_stock[,-c("delta","new_opening")], 
                             id.vars = c("CPCCode","Year"),
                             variable.name = "Flag_Des", value.name = "Flag"))
  FlagData[, Flag_Des := NULL]
  stockData <- cbind(ValueData, FlagData[,"Flag"])
  stockData <- rbind(stockData, data_2013[,c("CPCCode","ElementCode","Year","Value","Flag"),with = F])
  stockData <- merge(stockData,all_cpc, by="CPCCode",all.x = T)
  stockData <- merge(stockData,all_elements, by="ElementCode",all.x = T)
  stockData <- stockData[order(CPCCode,Year)]
  stockData <-wide_format(stockData)
flagcols <- grep("^Flag", names(stockData), value = TRUE)
yearcols <- grep("^[[:digit:]]{4}$", names(stockData), value = TRUE)
  
minyear <- min(as.numeric(yearcols))
maxyear <- max(as.numeric(yearcols))
  
if(input$endyear > maxyear +1){
    yearsToFill = (maxyear + 1):input$endyear
    
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
    stockData <- visualize_data(stockData,input, session)
    stockData <-stockData[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
    stockData[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_stock <- stockData
}
  })
  
  }