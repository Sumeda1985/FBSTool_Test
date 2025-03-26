
save_to_database <- function(data ,year_range,session,input,output){
  
  country_code  <- unique(value_database$data$CountryM49)
  print(country_code)
  country  <- unique(value_database$data$Country)
  element <- unique(data$ElementCode)
  cpc <- unique(data$CPCCode)
  
  
  data <- long_format(data)
  data[, c("Commodity","Element") := NULL]
  data[,ElementCode := as.character(ElementCode)]
  
 database <- copy(value_database$data)
 database[, c("CountryM49","Country", "Commodity","Element") := NULL]
 new_data <-merge(database,data, by=c("CPCCode","ElementCode","Year"), all = TRUE)
 new_data[, Value.x := ifelse(ElementCode %in% element & Year %in% year_range & CPCCode %in% cpc, Value.y,Value.x)]
 new_data[, Flag.x := ifelse(ElementCode %in% element & Year %in% year_range & CPCCode %in% cpc, Flag.y,Flag.x)]
 new_data[,c("Value.y","Flag.y") := NULL]
 setnames(new_data, c("Value.x","Flag.x"),c("Value","Flag"))
 new_data[, CountryM49 := country_code]
 new_data[, Country := country]
 new_data <- merge(new_data,all_cpc, by = "CPCCode", all.x = TRUE)
 new_data <- merge(new_data,all_elements_to_merge, by = "ElementCode", all.x = TRUE)
 setcolorder(new_data, c("CountryM49","Country","CPCCode", "Commodity","ElementCode","Element","Year","Value","Flag"))
 new_data[, Flag := ifelse(!is.na(Value) & is.na(Flag), "", Flag)]
 showNotification("Trying to write to sqlite")    
 #SQL database 
   dbWriteTable(con, "dbcountry", new_data,overwrite = TRUE)
   value_database$data <<- new_data


}




