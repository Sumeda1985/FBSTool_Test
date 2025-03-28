
save_to_database <- function(data ,year_range,session,input,output){
    country_code  <- countrycode(input$countrym49, origin = 'country.name', destination = 'un')
    country  <- input$countrym49
    element <- unique(data$ElementCode)
    cpc <- unique(data$CPCCode)
    data <- long_format(data)
    data[, c("Commodity","Element") := NULL]
    
    data[, CountryM49 := as.character(country_code)]
    col_format <- c("ElementCode", "Year", "CPCCode")
    data[, (col_format) := lapply(.SD, as.character), .SDcols = (col_format)]
    
   ## database <- copy(value_database$data)
    ## database[, c("CountryM49","Country", "Commodity","Element") := NULL]
    new_data <-merge(value$cropDataLong[,c("Commodity","Element") := NULL][, CountryM49 := as.character(country_code)]
                           [, (col_format) := lapply(.SD, as.character), .SDcols = (col_format)],
                     data,
                     by=c("CountryM49","CPCCode","ElementCode","Year"), all = TRUE)
    old_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                          .(CountryM49,
                            Country=countrycode(as.numeric(CountryM49), 
                                                destination = 'country.name', 
                                                origin = 'un'),
                            CPCCode,
                            ElementCode, 
                            Year,
                            StatusFlag=0,
                            LastModified,
                            Value=Value.x,
                            Flag=Flag.x)]
    new_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                        .(CountryM49,
                          Country=countrycode(as.numeric(CountryM49), 
                                              destination = 'country.name', 
                                              origin = 'un'),
                          CPCCode, ElementCode, Year,
                          StatusFlag=1,
                          LastModified=as.numeric(Sys.time()),
                          Value=Value.y,
                          Flag=Flag.y)]
    old_data <- merge(old_data, all_cpc, by = "CPCCode", all.x=TRUE)
    new_data <- merge(new_data, all_cpc, by = "CPCCode", all.x=TRUE)
    old_data <- merge(old_data, all_elements, by = "ElementCode", all.x=TRUE)
    new_data <- merge(new_data, all_elements, by = "ElementCode", all.x=TRUE)
    rows_update(contbl, as_tibble(old_data), 
                by = c("CountryM49", "Country",
                       "CPCCode", "Commodity",
                       "ElementCode","Element",
                       "Year", "Value", "Flag",
                       "LastModified"),
                in_place=TRUE, copy = TRUE, unmatched="ignore")
    browser()
    rows_update(contbl, as_tibble(value$dropcropdata), 
                by = c("CountryM49", "Country",
                       "CPCCode",
                       "ElementCode"),
                in_place=TRUE, copy = TRUE, unmatched="ignore")
    value$dropcropdata <- value$dropcropdata[0,]    
    rows_insert(contbl, as_tibble(new_data), 
                by = c("CountryM49",
                       "CPCCode","ElementCode",
                       "Year", "Value", "Flag",
                       "LastModified"),
                in_place=TRUE, copy = TRUE, conflict = "ignore")
}
