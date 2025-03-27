
save_to_database <- function(data ,year_range,session,input,output){
    # browser()
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
                            CPCCode, ElementCode, Year,
                            StatusFlag=0,
                            LastModified,
                            Value=Value.x,
                            Flag=Flag.x)]
    new_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                        .( CountryM49,
                          CPCCode, ElementCode, Year,
                          StatusFlag=1,
                          LastModified=as.numeric(Sys.time()),
                          Value=Value.y,
                          Flag=Flag.y)]
    rows_update(contbl, as_tibble(old_data), ##if here needs Commodity , Country and Element columns to use row_update, 
                #then new_data has to be merged with all_cpc all_elements and create new column with "Country := country"
                by = c("CountryM49",
                       "CPCCode","ElementCode"
                       , "Year", "Value", "Flag",
                       "LastModified"),
                in_place=TRUE, copy = TRUE, unmatched="ignore")
    rows_insert(contbl, as_tibble(new_data), 
                by = c("CountryM49",
                       "CPCCode","ElementCode"
                       , "Year", "Value", "Flag",
                       "LastModified"),
                in_place=TRUE, copy = TRUE, conflict = "ignore")
}
