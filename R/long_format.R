long_format =function(data){
  tempdata <- copy(data)
  # Get column names and convert data types
  flagcols <- grep("^Flag", names(tempdata), value = TRUE)
  yearcols <- grep("^[[:digit:]]{4}$", names(tempdata), value = TRUE)
  tempdata[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
  tempdata[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]
  # Melt data for values and flags
  tempdataValue <- melt.data.table(tempdata, id.vars = c("CPCCode", "Commodity", "ElementCode", "Element"), 
                                   measure.vars = yearcols, value.name = "Value")[, variable := as.character(variable)]
  setnames(tempdataValue, "variable", "Year")
  tempdataFlag <- melt.data.table(tempdata, id.vars = c("CPCCode", "Commodity", "ElementCode", "Element"),
                                  measure.vars = flagcols, value.name = "Flag")[, variable := as.character(substring(variable, 6))]
  setnames(tempdataFlag, "variable", "Year")
  # Merge and return
  return(merge(tempdataValue, tempdataFlag, by = intersect(names(tempdataValue), names(tempdataFlag))))
 }