#' Table History Management
#' 
#' This module provides functionality for managing table versions with undo/redo
#' capabilities. It maintains a history of table states and allows reverting to
#' previous versions.
#' 
#' @keywords internal
#' @importFrom data.table copy

# Global table history storage
.TableHistory <- list()

#' Add a New Version to Table History
#' 
#' Adds a new version of a table to its history.
#' 
#' @param name The name of the table
#' @param value The new version of the table to add
#' @export
Add_table_version <- function(name, value) {
  .TableHistory[[name]] <<- c(.TableHistory[[name]], list(value))
}

#' Pop the Latest Version from Table History
#' 
#' Removes the latest version of a table from its history and returns the
#' previous version.
#' 
#' @param name The name of the table
#' @return The previous version of the table, or NULL if no history exists
#' @export
Pop_table_version <- function(name) {
  if (is.null(.TableHistory[[name]])) {
    warning(sprintf("No table with name '%s' found in .TableHistory", name))
    return(NULL)
  }
  
  versions <- length(.TableHistory[[name]])
  if (versions == 1) {
    warning(sprintf("No history to reset for '%s'", name))
    return(NULL)
  }
  
  .TableHistory[[name]][versions] <<- NULL
  last_version <- copy(.TableHistory[[name]][[versions - 1]])
  return(last_version)
}
