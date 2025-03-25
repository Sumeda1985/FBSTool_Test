long_format =function(data){
  
  
tempdata=copy(data)  
  
  
flagcols <- grep("^Flag", names(tempdata), value = TRUE)
yearcols<- grep("^[[:digit:]]{4}$", names(tempdata), value = TRUE)


tempdata[, (flagcols) := lapply(.SD, as.character), .SDcols = flagcols]
tempdata[, (yearcols) := lapply(.SD, as.numeric), .SDcols = yearcols]




tempdataValue= melt.data.table(tempdata, id.vars = c("CPCCode", "Commodity","ElementCode" ,"Element"), measure.vars =grep("^[[:digit:]]{4}$", names(tempdata), value = TRUE),
                     value.name= "Value")
setnames(tempdataValue,"variable", "Year")

tempdataValue[,Year:= as.character(Year)]
tempdataFlag= melt.data.table(tempdata, id.vars = c( "CPCCode", "Commodity","ElementCode" ,"Element"),
                     measure.vars =grep("^Flag", names(tempdata), value = TRUE),
                     value.name= "Flag" )

setnames(tempdataFlag,"variable", "Year")
tempdataFlag[,Year:=substring(Year,6)]
tempdataFlag[,Year:=as.character(Year)]

tempdataFinal=merge(tempdataValue, tempdataFlag, by = intersect(names(tempdataValue), names(tempdataFlag)))

return(tempdataFinal)  
  
  
  
}