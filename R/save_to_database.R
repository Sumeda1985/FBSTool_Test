#' Save Data to Database
#' 
#' This function saves data to the database, handling updates, deletions, and insertions.
#' It manages both old and new data versions with proper status flags and timestamps.
#' 
#' @param data The data to save
#' @param year_range The range of years to process
#' @param session The Shiny session object
#' @param input The Shiny input object
#' @param output The Shiny output object
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom countrycode countrycode
#' @importFrom tibble as_tibble
#' @importFrom dplyr rows_update
#' @importFrom dplyr rows_delete
#' @importFrom dplyr rows_insert
save_to_database <- function(data,longData, year_range, session, input, output,data_session) {
 # Convert data to long format and prepare columns
  data <- long_format(data)
  data[, c("Commodity", "Element") := NULL]
  col_format <- c("ElementCode", "Year", "CPCCode")
  data[, (col_format) := lapply(.SD, as.character), .SDcols = (col_format)]
 # Merge new and old data frames
  new_data <- merge(
    longData[, (col_format) := lapply(.SD, as.character),
                       .SDcols = (col_format)],
    data,
    by = c("CPCCode", "ElementCode", "Year"),
    all = TRUE
  )
  
  # Prepare old data for records that have changed
  old_data <- new_data[Value.y != Value.x | Flag.y != Flag.x|is.na(Value.y != Value.x)|is.na(Flag.y != Flag.x) ,
                       .(CountryM49 = as.character(
                         countrycode(input$countrym49,
                                   origin = "country.name",
                                   destination = "un")),
                         Country = input$countrym49,
                         CPCCode,
                         ElementCode,
                         Year,
                         StatusFlag = 0,
                         LastModified,
                         Value = Value.x,
                         Flag = Flag.x)]
  
# Prepare new data for records that have changed
  new_data <- new_data[Value.y != Value.x | Flag.y != Flag.x |is.na(Value.y != Value.x)|is.na(Flag.y != Flag.x),
                       .(CountryM49 = as.character(
                         countrycode(input$countrym49,
                                     origin = "country.name",
                                     destination = "un")),
                         Country = input$countrym49,
                         CPCCode,
                         ElementCode,
                         Year,
                         StatusFlag = 1,
                         LastModified = as.numeric(Sys.time()),
                         Value = Value.y,
                         Flag = Flag.y)]
  
  # Merge with CPC and element codes
  old_data <- merge(old_data, all_cpc, by = "CPCCode", all.x = TRUE)
  new_data <- merge(new_data, all_cpc, by = "CPCCode", all.x = TRUE)
  old_data <- merge(old_data, all_elements, by = "ElementCode", all.x = TRUE)
  new_data <- merge(new_data, all_elements, by = "ElementCode", all.x = TRUE)
  
  # Update existing records
  rows_update(
    contbl,
    as_tibble(new_data),
    by = c("CountryM49",
           "CPCCode",
           "ElementCode", 
           "Year"),
    in_place = TRUE,
    copy = TRUE,
    unmatched = "ignore"
  )
  updateSUA <- updateSUA(input,output,session)
# Handle deletions
  if (!is.null(nrow(value$dropdata))) {
  rows_delete(
      contbl,
      as_tibble(value$dropdata),
      by = c("CountryM49", "Country",
             "CPCCode", "ElementCode"),
      in_place = TRUE,
      copy = TRUE,
      unmatched = "ignore"
    )
    value$dropdata <- value$dropdata[0, ]
    value_database$data <- value$dropdata
    updateSUA <- updateSUA(input,output,session)
  } else {
    value$dropdata <- NULL
  }
# Handle insertions
  if (!is.null(nrow(value$insertdata))) {
 insertData <- long_format(
      data_session[CPCCode %in% unique(value$insertdata$CPCCode) &
                     ElementCode %in% (value$insertdata$ElementCode)]
    )
    insertData[, `:=`(
      CountryM49 = as.character(
        countrycode(input$countrym49,
                    origin = "country.name",
                    destination = "un")
      ),
      Country = input$countrym49,
      StatusFlag = 1,
      LastModified = as.numeric(Sys.time())
    )]
    
    # Insert new records
    rows_insert(
      contbl,
      as_tibble(insertData),
      by = c("CountryM49",
             "CPCCode",
             "ElementCode",
             "Year"
        ),
      in_place = TRUE,
      copy = TRUE,
      conflict = "ignore"
    )
    
    #value_database$data <- value$insertdata
    
    updateSUA <- updateSUA(input,output,session)
  } else {
    value$insertdata <- NULL
  }
  
}
