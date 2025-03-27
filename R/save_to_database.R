
save_to_database <- function(data ,year_range,session,input,output){
    browser()
    country_code  <- unique(value_database$data$CountryM49)
    print(country_code)
    country  <- unique(value_database$data$Country)
    element <- unique(data$ElementCode)
    cpc <- unique(data$CPCCode)
    data <- long_format(data)
    data[, c("Commodity","Element") := NULL]
    data[,ElementCode := as.character(ElementCode)]
    ## database <- copy(value_database$data)
    ## database[, c("CountryM49","Country", "Commodity","Element") := NULL]
    new_data <-merge(value$cropDataLong,data, by=c("CPCCode","ElementCode","Year"), all = TRUE)
    old_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                          .(CountryM49,Country, 
                            CPCCode, ElementCode, Year,
                            Commodity, Element, 
                            StatusFlag=0,
                            LastModified,
                            Value=Value.x,
                            Flag=Flag.x)]
    new_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                        .(CountryM49,Country, 
                          CPCCode, ElementCode, Year,
                          Commodity, Element, 
                          StatusFlag=1,
                          LastModified=as.numeric(Sys.time()),
                          Value=Value.y,
                          Flag=Flag.y)]
    rows_update(contbl, as_tibble(old_data), 
                by = c("CountryM49","Country",
                       "CPCCode","Commodity","ElementCode",
                       "Element", "Year", "Value", "Flag",
                       "LastModified"),
                in_place=TRUE, copy = TRUE, unmatched="ignore")
    rows_insert(contbl, as_tibble(new_data), 
                by = c("CountryM49","Country",
                       "CPCCode","Commodity","ElementCode",
                       "Element", "Year", "Value", "Flag",
                       "LastModified"),
                in_place=TRUE, copy = TRUE, conflict = "ignore")
    
    new_data[,`:=`(Value.x=NULL,
                   Flag.x=N)]
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
   
   


}
