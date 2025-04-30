upload_denormalized_data <- function(input, session, value, all_elements, all_cpc, file_input, data_key, version_label,value_env) {
  observeEvent(input[[file_input]], {
    inFile <- input[[file_input]]
    if (is.null(inFile)) return(NULL)
    
    # Fix file extension and read
    file_path <- paste0(inFile$datapath, ".xlsx")
    file.rename(inFile$datapath, file_path)
    
    new_data <- data.table(read_excel(file_path, 1))
    new_data <- long_format(new_data)
    
    # Ensure expected columns exist
    required_cols <- c("Year", "ElementCode", "CPCCode", "Value", "Flag")
    if (!all(required_cols %in% names(new_data))) {
      showNotification("Uploaded file is missing required columns.", type = "error")
      return(NULL)
    }
    
    # Filter by year and clean up
    new_data <- new_data[Year %in% input$fromyear:input$endyear]
    new_data[, c("Commodity", "Element") := NULL]
    new_data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
   # Prepare existing data
    existing_data <- data.table(value_env[[data_key]])[,hidden := NULL]
    existing_data <- long_format(existing_data)
    existing_data[, c("Commodity", "Element") := NULL]
    existing_data[, c("ElementCode", "CPCCode") := lapply(.SD, as.character), .SDcols = c("ElementCode", "CPCCode")]
    
    # Perform update
    update <- existing_data[!is.na(Value)][new_data, on = c("CPCCode", "ElementCode", "Year")]
    update[, c("Value", "Flag") := NULL]
    setnames(update, c("i.Value", "i.Flag"), c("Value", "Flag"))
    
    final_data <- rbind(
      existing_data[!update, on = c("CPCCode", "ElementCode", "Year")],
      update
    )
    
    # Merge reference data
    final_data <- merge(final_data, all_elements, by = "ElementCode", all.x = TRUE)
    final_data <- merge(final_data, all_cpc, by = "CPCCode", all.x = TRUE)
    
    # Final formatting
    final_data <- final_data[!is.na(Element) & Year %in% 2010:input$endyear]
    final_data <- wide_format(final_data)
    final_data <- visualize_data(final_data, input,session)
    final_data[, hidden := fifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    
    # Store and version
    value_env[[data_key]] <- final_data
    Add_table_version(version_label, copy(final_data))
  })
}