#' Convert a normalized table to a denormalized table
#' @param data A data.table containing food consumption data
#' @return A data.table in wide format with values and flags
wide_format = function(data) {
  # Input validation
  if (!is.data.table(data)) {
    stop("Input must be a data.table")
  }

  required_cols = c("CPCCode", "Commodity", "ElementCode", "Element", "Year", "Value", "Flag")
  missing_cols = setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Cast data from long to wide format
  data = tryCatch(
    dcast.data.table(data, CPCCode+Commodity+ElementCode+Element ~ Year,
                    value.var = c("Value", "Flag")),
    error = function(e) stop("Error in reshaping data: ", e$message)
  )

  # Store column patterns
  flag_pattern = "^Flag"
  value_pattern = "^Value"

  # Get column names once
  flag_cols = grep(flag_pattern, names(data), value = TRUE)
  value_cols = grep(value_pattern, names(data), value = TRUE)

  # Convert column types 
  data[, (flag_cols) := lapply(.SD, as.character),
       .SDcols = (flag_cols)]
  data[, (value_cols) := lapply(.SD, as.numeric),
       .SDcols = (value_cols)]

  # Prepare new column names
  new_flag_names = gsub("_", " ", flag_cols)
  new_value_names = gsub("^.*?_", "", value_cols)

  # Reorder and rename columns in one step
  new_order = c("CPCCode", "Commodity", "ElementCode", "Element",
                as.vector(rbind(value_cols, flag_cols)))
  setcolorder(data, new_order)
  setnames(data,
           old = c(flag_cols, value_cols),
           new = c(new_flag_names, new_value_names))

  # Return sorted data
  data[order(CPCCode)]
}
