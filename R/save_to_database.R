save_to_database <- function(data ,year_range,session,input,output){
  data <- long_format(data)
  data[, c("Commodity","Element") := NULL]
  col_format <- c("ElementCode", "Year", "CPCCode")
  data[, (col_format) := lapply(.SD, as.character), .SDcols = (col_format)]
  #merging new and old data frames
  new_data <-merge(value$cropDataLong[, (col_format) := lapply(.SD, as.character),
                                      .SDcols = (col_format)],
                   data,
                   by=c("CPCCode","ElementCode","Year"), all = TRUE) 
  old_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                       .(CountryM49 = as.character(
                         countrycode(input$countrym49, 
                                     origin = 'country.name', 
                                     destination = 'un')),
                         Country=input$countrym49,
                         CPCCode,
                         ElementCode, 
                         Year,
                         StatusFlag=0,
                         LastModified,
                         Value=Value.x,
                         Flag=Flag.x)]
  new_data <- new_data[Value.y!=Value.x|Flag.y!=Flag.x,
                       .(CountryM49=as.character(CountryM49),
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
  
  #update an existing record
  rows_update(contbl, as_tibble(new_data), 
              by = c("CountryM49", "Country", 
                     "CPCCode", "Commodity",
                     "ElementCode","Element",
                     "Year" ),
              in_place=TRUE, copy = TRUE, unmatched="ignore")
  #delete an existing record
  if(!is.null(nrow(value$dropcropdata))){
    rows_delete(contbl, as_tibble(value$dropcropdata), 
                by = c("CountryM49", "Country",
                       "CPCCode",
                       "ElementCode"),
                in_place=TRUE, copy = TRUE, unmatched="ignore")
    value$dropcropdata <- value$dropcropdata[0,]    
  } else {
    value$dropcropdata <- NULL
  }
  
  if (!is.null(nrow(value$insertcropdata))){
    insertData <- long_format(
      value$data_crop[CPCCode %in% unique(value$insertcropdata$CPCCode) & 
                        ElementCode %in% (value$insertcropdata$ElementCode)])
    insertData[,`:=`
               (CountryM49 = as.character(
                 countrycode(input$countrym49, 
                             origin = 'country.name', 
                             destination = 'un')),
                 Country=input$countrym49,
                 StatusFlag=1,
                 LastModified = as.numeric(Sys.time())
               )] 
    #inserting a new record
    rows_insert(contbl, as_tibble(insertData), 
                by = c("CountryM49",
                       "CPCCode",
                       "ElementCode",
                       "Year", "Value", "Flag",
                       "LastModified", "StatusFlag"),
                in_place=TRUE, copy = TRUE, conflict = "ignore")
  } else {
    value$insertcropdata <- NULL
  }
  
  
  
  
  
  
  
}
