normalized_upload <- function(data_source, input_cols, std_names, filter_element_code,
                           from_year, to_year, existing_data, all_elements, all_cpc,
                           value_env, value_name, version_label,input, session) {
  # Get the uploaded data
  data <- data.table(value_env[[data_source]])
  print(input_cols)
  # Validate required column selections
  if (any(sapply(input_cols, function(x) input[[x]] == ""))) {
    sendSweetAlert(
      session = session,
      title = "Warning",
      text = "Please select all required columns",
      type = "warning"
    )
    return()
  }
  
  # Select and validate columns
  selected_cols <- sapply(input_cols, function(x) input[[x]])
  data <- data[, selected_cols, with = FALSE]
  
  # Check for duplicate column names
  if (length(names(data)[duplicated(names(data))]) > 0) {
    sendSweetAlert(
      session = session,
      title = "Warning",
      text = "Please select unique columns",
      type = "warning"
    )
    return()
  }
  
  # Rename columns to standard names
  setnames(data, selected_cols, std_names)
  
  # Filter data
  data <- data[ElementCode %in% filter_element_code]
  
  # Convert columns to character type
  data[, c("Year", "CPCCode", "ElementCode") := lapply(.SD, as.character),
       .SDcols = c("Year", "CPCCode", "ElementCode")]
  
  # Filter by year range
  data <- data[Year %in% c(from_year:to_year)]
  
  # Process existing data
  dataVar <- data.table(value_env[[existing_data]])
  dataVar <- long_format(dataVar)
  dataVar[, c("Commodity", "Element") := NULL]
  dataVar[, ElementCode := as.character(ElementCode)]
  
  # Merge new data with existing data
  new_data <- dataVar[!is.na(Value)][data, on = c("CPCCode", "ElementCode", "Year")]
  new_data[, c("Value", "Flag") := NULL]
  setnames(new_data, c("i.Value", "i.Flag"), c("Value", "Flag"))
  
  dataVar <- dataVar[!is.na(Value)][!new_data, on = c("CPCCode", "ElementCode", "Year")]
  dataVar <- rbind(dataVar, new_data)
  
  # Merge with reference data
  dataVar <- merge(dataVar, all_elements, by = "ElementCode", all.x = TRUE)
  dataVar <- merge(dataVar, all_cpc, by = "CPCCode", all.x = TRUE)
  dataVar <- dataVar[!is.na(Element)]
  
  # Convert to wide format and add hidden column
  dataVar <- wide_format(dataVar)
  dataVar[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  
  # Update values and save version
  value_env[[value_name]] <- dataVar
  Add_table_version(version_label, copy(dataVar))
}



