tradeImportMapping <- function(input, output, session) {
  # Get year range and import data
  t <- as.numeric(input$fromyear:input$endyear)
  importData <- data.table(value$data_importsCountry)
  
  # Select and rename columns
  cols <- if(input$importCommodity != "") {
    c(input$importYear, input$importHS, input$importQuantity, input$importCommodity)
  } else {
    c(input$importYear, input$importHS, input$importQuantity)
  }
  importData <- importData[, cols, with = FALSE]
  setnames(importData, cols, if(input$importCommodity != "") {
    c("Year", "HS6", "Value", "Commodity")
  } else {
    c("Year", "HS6", "Value")
  })
  
  # Filter by year
  importData <- importData[Year %in% t]
  
  # Check if data exists
  if (nrow(importData) == 0) {
    sendSweetAlert(session, "No data to map !!", "No data to map", "warning")
    return(value$data_imports)
  }
  
  # Process HS codes
  importData$HS6 <- substr(importData$HS6, 1, 6)
  importData[, c("HS6", "Year") := lapply(.SD, as.character), .SDcols = c("HS6", "Year")]
  importData[, Value := as.numeric(Value)]
  
  # Read trade map
  tradeMap <- data.table(readRDS("Data/tradeMap_2019.rds"))[!cpc == ""]
  tradeMap[, HS6 := substr(hs, 1, 6)]
  tradeMap[, lenght_hs := nchar(hs)]
  
  # Get import map
  importMap <- tradeMap[flow == 1]
  
  # Filter and aggregate import data
  importData <- importData[!is.na(HS6)]
  group_cols <- if(input$importCommodity != "") {
    c("HS6", "Year", "Commodity")
  } else {
    c("HS6", "Year")
  }
  importData <- importData[, .(Value = sum(Value)), by = group_cols]
  
  # Merge with trade map
  importData <- merge(importData, unique(importMap[, c("HS6", "cpc"), with = FALSE]), 
                      by = "HS6", all.x = TRUE)
  
  # Handle unmapped commodities
  not_mapped_cols <- if(input$importCommodity != "") {
    c("HS6", "Commodity", "Value")
  } else {
    c("HS6", "Value")
  }
  importData_Not_mapped <- importData[is.na(cpc)][,c(not_mapped_cols),with = F]
  value$data_imports_not_mapped <- importData_Not_mapped[!is.na(Value)]
  
  # Filter mapped data
  importData <- importData[!is.na(cpc)]
  if(input$importCommodity != "") {
    importData[, Commodity := NULL]
  }
  
  # Check if mapped data exists
  if (nrow(importData) == 0) {
    sendSweetAlert(session, "WARNING !!", "There are no CPC codes mapped to the provided HS Codes", "warning")
    return(value$data_imports)
  }
  
  # Aggregate and prepare data
  importData <- data.table(aggregate(Value ~ Year + cpc, importData, sum, na.rm = TRUE))
  importData <- importData[!duplicated(importData)][, Flag := ""]
  setnames(importData, "cpc", "CPCCode")
  
  # Process trade import years
  trade_import_years <- data.table(value$data_imports)[, hidden := NULL]
  flagcols <- grep("^Symbole", names(trade_import_years), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(trade_import_years), value = TRUE)
  
  # Convert data types
  trade_import_years[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  trade_import_years[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  # Melt data
  trade_import_years <- melt.data.table(
    trade_import_years, 
    id.vars = c("CPCCode", "Commodity", "ElementCode", "Element"), 
    measure.vars = yearcols,
    value.name = "Value"
  )
  
  # Filter and prepare data
  trade_import_years <- value_database$data[ElementCode == "5610"][,c("ElementCode", "CountryM49", 
                                            "Country", "Commodity", "Element","StatusFlag","LastModified"):= NULL]
  trade_import_years <- trade_import_years[!Year %in% t]
 # Merge data
  finalData <- rbind(trade_import_years, importData)
  
  # Create data to merge
  data_to_merge <- data.table(expand.grid(
    CPCCode = as.character(unique(finalData$CPCCode)), 
    Year = as.character(2010:t[length(t)])
  ))
  
  # Final merge and processing
  finalData <- merge(data_to_merge, finalData, by = c("CPCCode", "Year"), all.x = TRUE)
  finalData[, ElementCode := "5610"]
  finalData <- merge(finalData, all_cpc, by = "CPCCode", all.x = TRUE)
  finalData <- merge(finalData, all_elements, by = "ElementCode", all.x = TRUE)
  # Return wide format
  return(wide_format(finalData))
}