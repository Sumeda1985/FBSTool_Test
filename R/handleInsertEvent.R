handleInsertEvent <- function(input_rows, element_codes, value_store, data_type) {
  selected_rows <- as.numeric(input_rows)
  data_name <- paste0("data_", data_type)
if (length(selected_rows) == 0) {
    value_store[[data_name]] <- data.table(value_store[[data_name]])
    return()
  }
  
  # Get CPCs not already in the data
  dataVar <- copy(all_cpc)[order(CPCCode)][
    !CPCCode %in% unique(isolate(value_store[[data_name]]$CPCCode))
  ][selected_rows, c("CPCCode", "Commodity")]
  
  # Expand rows for each element code
  element_dt <- data.table(CPCCode = dataVar$CPCCode, ElementCode = element_codes)
  
  if (nrow(dataVar) > 0 && nrow(element_dt) > 0) {
    row_add <- merge(dataVar, element_dt, by = "CPCCode", allow.cartesian = TRUE)
  } else {
    row_add <- data.table()
  }
  
  # Merge element names
  if ("ElementCode" %in% names(row_add) && "ElementCode" %in% names(all_elements)) {
    row_add <- merge(row_add, all_elements, by = "ElementCode", all.x = TRUE)
    setcolorder(row_add, c("CPCCode", "Commodity", "ElementCode", "Element"))
  }
  
  # Load and clean existing data
  data <- data.table(isolate(value_store[[data_name]]))[, hidden := NULL]
  
  # Avoid duplicates
  new_rows <- row_add[!paste(CPCCode, ElementCode) %in% paste(data$CPCCode, data$ElementCode)]
  
  if (nrow(new_rows) > 0) {
    data <- rbind(new_rows, data, fill = TRUE)[!is.na(ElementCode)]
    data[is.na(data)] <- ""
    yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  }
  
  # Store and update
  value_store$insertdata <- new_rows
  data[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value_store[[data_name]] <- data
  
  Add_table_version(data_type, copy(value_store[[data_name]]))
}