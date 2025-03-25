#This function convert a nomarlized table to a denormalized table. 



wide_format=function(data){

  #check for data
  stopifnot((names(data) %in% c("CPCCode","Commodity","ElementCode", "Element","Year", "Value", "Flag")))
   
  data =  dcast.data.table(data, CPCCode+Commodity+ElementCode+Element ~ Year, value.var = c("Value","Flag"))
  
  flagcols <- grep("^Flag", names(data), value = TRUE)
  yearcols <- grep("^Value", names(data), value = TRUE) 
  
  
  #year and flag vectors should have the same length
  
  
  stopifnot(length(flagcols) == length(yearcols))
  
  flagcols_new=gsub("_", " ", flagcols, fixed=TRUE)
  
  yearcols_new=gsub("^.*?_","",yearcols)
  
  setnames(data,flagcols, flagcols_new)
  setnames(data,yearcols, yearcols_new)
  
  addorder <- as.vector(rbind(yearcols_new, flagcols_new))
  
  
  
  setcolorder(data,c("CPCCode","Commodity","ElementCode","Element",addorder))
  
  
  data[, (flagcols_new) := lapply(.SD, as.character), .SDcols = flagcols_new]
  data[, (yearcols_new) := lapply(.SD, as.numeric), .SDcols = yearcols_new]
  
  data[order(CPCCode)]
  
  return(data)
  
}