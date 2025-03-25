.TableHistory <- list()

Add_table_version <- function(name, value) {
  
  .TableHistory[[name]] <<- c(.TableHistory[[name]], list(value))

}

Pop_table_version <- function(name) {
  
  if(is.null(.TableHistory[[name]])) {
    cat(paste0("Warning - no table with name `", name, "` found in .TableHistory\n"))
    return(NULL)
  }
  
  versions <- length(.TableHistory[[name]])
  
  if(versions == 1) {
    cat(paste0("Warning - no history to reset for `", name, "`\n"))
    return(NULL)
  }

  .TableHistory[[name]][versions] <<- NULL
  last_version <- copy(.TableHistory[[name]][[versions-1]])
  
  return(last_version)
}

