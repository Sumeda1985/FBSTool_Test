tradeExportMapping <- function(input, output, session, values_exportCountryData) {
  # Get year range and process export data
  years <- as.numeric(input$fromyear):as.numeric(input$endyear)
  exportData <- data.table(value$data_exportsCountry)
  
  # Select and rename columns
  cols_to_select <- c(input$exportYear, input$exportHS, input$exportQuantity)
  if (input$exportCommodity != "") cols_to_select <- c(cols_to_select, input$exportCommodity)
  
  exportData <- exportData[, cols_to_select, with = FALSE]
  new_names <- c("Year", "HS6", "Value")
  if (input$exportCommodity != "") new_names <- c(new_names, "Commodity")
  setnames(exportData, cols_to_select, new_names)
  
  # Filter by year range and check if data exists
  exportData <- exportData[Year %in% years]
  if (nrow(exportData) == 0) {
    sendSweetAlert(session, "No data to map !!", "No data to map", "warning")
    return(value$data_exports)
  }
  
  # Process HS codes and load trade map
  exportData$HS6 <- substr(exportData$HS6, 1, 6)
  exportData[, c("HS6", "Year") := lapply(.SD, as.character), .SDcols = c("HS6", "Year")]
  exportData[, Value := as.numeric(Value)]
  
  tradeMap <- data.table(dbReadTable(concore, name="trade_map"))[!cpc == ""]
  tradeMap[, HS6 := substr(hs, 1, 6)]
  exportMap <- tradeMap[flow == 2 & !is.na(HS6)]
  
  # Aggregate and merge with trade map
  group_by_cols <- c("HS6", "Year")
  if (input$exportCommodity != "") group_by_cols <- c(group_by_cols, "Commodity")
  exportData <- exportData[, .(Value = sum(Value)), by = group_by_cols][!is.na(HS6)]
  
  exportData <- merge(exportData, unique(exportMap[, c("HS6", "cpc"), with = FALSE]), 
                      by = "HS6", all.x = TRUE)
  
  # Handle unmapped commodities
  not_mapped_cols <- if(input$exportCommodity != "") {
    c("HS6", "Commodity", "Value")
  } else {
    c("HS6", "Value")
  }
  exportData_Not_mapped <- exportData[is.na(cpc)][,c(not_mapped_cols),with = F]
  value$data_exports_not_mapped <- exportData_Not_mapped[!is.na(Value)]
  
  # Filter mapped data
  exportData <- exportData[!is.na(cpc)]
  if (input$exportCommodity != "") exportData[, Commodity := NULL]
  
  # Check if any data is mapped
  if (nrow(exportData) == 0) {
    sendSweetAlert(session, "WARNING !!", "There are no CPC codes mapped to the provided HS Codes", "warning")
    return(value$data_exports)
  }
  
  # Aggregate by year and CPC code
  exportData <- data.table(aggregate(Value ~ Year + cpc, exportData, sum, na.rm = TRUE))
  setnames(exportData, "cpc", "CPCCode")
  exportData <- exportData[!duplicated(exportData)][, Flag := ""]
  
  # Process trade export years data
  trade_export_years <- data.table(value$data_exports)[, hidden := NULL]
  flagcols <- grep("^Symbole", names(trade_export_years), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(trade_export_years), value = TRUE)
  
  trade_export_years[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  trade_export_years[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  # Melt and filter data
  trade_export_years <- melt.data.table(
    trade_export_years, 
    id.vars = c("CPCCode", "Commodity", "ElementCode", "Element"), 
    measure.vars = yearcols,
    value.name = "Value"
  )
  
  trade_export_years <- value_database$data[ElementCode == "5910"][, 
                     c("ElementCode", "CountryM49", "Country", "Commodity", "Element","StatusFlag","LastModified") := NULL
  ][!Year %in% years]
  
  # Combine data and finalize
  finalData <- rbind(trade_export_years, exportData)
  data_to_merge <- data.table(expand.grid(
    CPCCode = as.character(unique(finalData$CPCCode)), 
    Year = as.character(2010:years[length(years)])
  ))
  
  finalData <- merge(data_to_merge, finalData, by = c("CPCCode", "Year"), all.x = TRUE)[
    , ElementCode := "5910"
  ]
  
  finalData <- merge(merge(finalData, all_cpc, by = "CPCCode", all.x = TRUE), 
                     all_elements, by = "ElementCode", all.x = TRUE)
  
  return(wide_format(finalData))
}