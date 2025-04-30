createDownloadHandler <- function(data_source_reactive, filename) {
  downloadHandler(
    filename = function() {
      filename
    },
    content = function(file) {
      # Call the reactive expression to get actual data
      download_data <- data.table(data_source_reactive())
      
      # Filter and clean
      download_data <- download_data[!is.na(CPCCode)]
      download_data[, hidden := NULL]
      
      # Write Excel
      write.xlsx(download_data, file, row.names = FALSE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}