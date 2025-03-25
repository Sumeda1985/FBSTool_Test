

#data should be in wide format for this function. 

visualize_data_production=function(data,END_YEAR, session){
  
  
  
  flagcols <- grep("^Flag", names(data), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  
  minyear <- min(as.numeric(yearcols))
  maxyear <- max(as.numeric(yearcols))
  
  # Convert types
  data[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  data[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  
  
  
  # yearErrors(END_YEAR, minyear, maxyear,session)
  
  desired_yearset <- minyear:END_YEAR
  
  final_data <- if(maxyear == END_YEAR){
    
    data[]
    
  } else if(maxyear > END_YEAR){
    
    tempdata <- copy(data)
    delyears <- as.character((as.numeric(END_YEAR) + 1):maxyear)
    delflags <- paste("Flag", delyears)  
    
    tempdata[, c(delyears, delflags) := NULL]
    tempdata[]
    
  } else if(maxyear < END_YEAR){
    
    tempdata <- copy(data)
    # Get names for column ordering later
    prevnames <- names(data)
    
    addyears <- as.character(END_YEAR)
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