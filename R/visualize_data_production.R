

#data should be in wide format for this function. 

visualize_data_production=function(data,input ,session){
  
  flagcols <- grep("^Flag", names(data), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  # Convert types
  data[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  desired_yearset <- minyear:as.numeric(input$endyear)
  final_data <- if(maxyear == input$endyear){
    
    data[]
    
  } else if(maxyear > input$endyear){
    tempdata <- copy(data)
    delyears <- as.character((as.numeric(input$endyear) + 1):maxyear)
    delflags <- paste("Flag", delyears)  
    tempdata[, c(delyears, delflags) := NULL]
    tempdata[]
    
  } else if(maxyear < input$endyear){
    
    tempdata <- copy(data)
    # Get names for column ordering later
    prevnames <- names(data)
    addyears <- as.character(input$endyear)
    addflags <- paste("Flag", addyears)  
    
    #order columns year then flag
    addorder <- as.vector(rbind(addyears, addflags))
    tempdata[, (addyears) := NA_real_]
    tempdata[, (addflags) := NA_character_]
    setcolorder(tempdata, c(prevnames, addorder))
    tempdata[]
}
  
  return(final_data)
  
  
  
}