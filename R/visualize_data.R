#' Process Production Data for Visualization
#' 
#' @param data A data.table in wide format containing year columns and flag columns
#' @param input A list containing endyear parameter
#' @param session The current session object
#' @return A processed data.table with adjusted year columns
#' @importFrom data.table :=
#' @importFrom data.table setcolorder

visualize_data <- function(data, input, session) {
  # Input validation
  if (!is.data.table(data)) {
    stop("Input 'data' must be a data.table object")
  }
  if (is.null(input$endyear)) {
    stop("input$endyear must be specified")
  }
  # Identify year and flag columns
  flag_cols <- grep("^Flag", names(data), value = TRUE)
  year_cols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
  # Validate that we have year columns
  if (length(year_cols) == 0) {
    stop("No year columns found in the data")
  }
  # Calculate year range
  min_year <- min(as.numeric(year_cols))
  max_year <- max(as.numeric(year_cols))
  end_year <- as.numeric(input$endyear)
 # Convert column types
  data[
    , (flag_cols) := lapply(.SD, as.character), 
    .SDcols = flag_cols
  ]
  data[
    , (year_cols) := lapply(.SD, as.numeric), 
    .SDcols = year_cols
  ]
 # Process data based on year range
  if (max_year == end_year) {
    return(data)
  }
  final_data <- copy(data)
  if (max_year > end_year) {
    # Remove excess years
    years_to_remove <- as.character((end_year + 1):max_year)
    flags_to_remove <- paste("Flag", years_to_remove)
    final_data[
      , c(years_to_remove, flags_to_remove) := NULL
    ]
  } else if (max_year < end_year) {
    # Add missing years
    years_to_add <- as.character((max_year + 1):end_year)
    flags_to_add <- paste("Flag", years_to_add)
    # Add new columns
    final_data[, (years_to_add) := NA_real_]
    final_data[, (flags_to_add) := NA_character_]
    # Maintain column order: years followed by flags
    new_cols <- as.vector(rbind(years_to_add, flags_to_add))
    setcolorder(
      final_data, 
      c(names(data), new_cols)
    )
  }
  
  return(final_data)
} 