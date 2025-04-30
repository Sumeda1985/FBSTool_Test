
handle_cell_edit <- function(proxy, cell_edit_info, data, session, table_name, version_history = TRUE) {
  # Update the data with the new cell value
  i <- cell_edit_info$row
  j <- cell_edit_info$col + 1
  v <- cell_edit_info$value
  data[i, (j) := v]
  
  # Update the table display
  replaceData(proxy, data, resetPaging = FALSE, rownames = FALSE)
  
  # Colorize the edited cell
  runjs(colorizeCell(i, j, table_name))
  
  # Add to version history if requested
  if (version_history) {
    Add_table_version(table_name, copy(data))
  }
}



